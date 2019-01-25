package encry.network

import java.io.File

import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.utils.NetworkTimeProvider
import encry.view.history.EncryHistory
import encry.view.history.processors.payload.{BlockPayloadProcessor, EmptyBlockPayloadProcessor}
import encry.view.history.processors.proofs.{ADStateProofProcessor, FullStateProofProcessor}
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore

class AuxilaryHistoryHolder(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  val history: EncryHistory = readOrGenerate(settings, ntp)

  override def receive: Receive = ???

  def readOrGenerate(settings: EncryAppSettings, ntp: NetworkTimeProvider): EncryHistory = {

    val historyIndexDir: File = getHistoryIndexDir(settings)
    val historyObjectsDir: File = getHistoryObjectsDir(settings)
    val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
    val storage: HistoryStorage = new HistoryStorage(indexStore, objectsStore)

    val history: EncryHistory = (settings.node.stateMode.isDigest, settings.node.verifyTransactions) match {
      case (true, true) =>
        new EncryHistory with ADStateProofProcessor with BlockPayloadProcessor {
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (false, true) =>
        new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (true, false) =>
        new EncryHistory with ADStateProofProcessor with EmptyBlockPayloadProcessor {
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case m => throw new Error(s"Unsupported settings ADState=:${m._1}, verifyTransactions=:${m._2}, ")
    }
    history
  }

  def getHistoryIndexDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/auxHistory/index")
    dir.mkdirs()
    dir
  }

  def getHistoryObjectsDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/auxHistory/objects")
    dir.mkdirs()
    dir
  }

  override def postStop(): Unit = {
    logger.warn(s"Stopping AuxilaryHistoryHolder")
    history.closeStorage()
  }

}
