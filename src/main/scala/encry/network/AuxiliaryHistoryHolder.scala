package encry.network

import java.io.File
import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.ProgressInfo
import encry.modifiers.EncryPersistentModifier
import encry.network.AuxiliaryHistoryHolder._
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.storage.VersionalStorage
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.NetworkTimeProvider
import encry.view.history.EncryHistory
import encry.view.history.EncryHistory.{getHistoryIndexDir, getHistoryObjectsDir}
import encry.view.history.processors.payload.{BlockPayloadProcessor, EmptyBlockPayloadProcessor}
import encry.view.history.processors.proofs.{ADStateProofProcessor, FullStateProofProcessor}
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.iq80.leveldb.Options

class AuxiliaryHistoryHolder(settings: EncryAppSettings, ntp: NetworkTimeProvider, syncronizer: ActorRef)
  extends Actor with StrictLogging {

  val history: EncryHistory = AuxiliaryHistoryHolder.readOrGenerate(settings, ntp)

  override def preStart(): Unit = syncronizer ! AuxHistoryChanged(history)

  override def receive: Receive = {
    case Append(mod) =>
      history.append(mod)
      syncronizer ! AuxHistoryChanged(history)
    case ReportModifierValid(mod) =>
      history.reportModifierIsValid(mod)
      syncronizer ! AuxHistoryChanged(history)
    case ReportModifierInvalid(mod, progressInfo) =>
      history.reportModifierIsInvalid(mod, progressInfo)
      syncronizer ! AuxHistoryChanged(history)
  }

  override def postStop(): Unit = {
    logger.warn(s"Stopping AuxiliaryHistoryHolder")
    history.closeStorage()
  }

}

object AuxiliaryHistoryHolder {

  private def getHistoryIndexDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/auxHistory/index")
    dir.mkdirs()
    dir
  }

  private def getHistoryObjectsDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/auxHistory/objects")
    dir.mkdirs()
    dir
  }

  protected[AuxiliaryHistoryHolder] def readOrGenerate(settingsEncry: EncryAppSettings, ntp: NetworkTimeProvider): EncryHistory = {

    val historyIndexDir: File = getHistoryIndexDir(settingsEncry)
    val historyObjectsDir: File = getHistoryObjectsDir(settingsEncry)
    val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
    val levelDBInit = LevelDbFactory.factory.open(historyIndexDir, new Options)
    val versionalStorage = VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settingsEncry.levelDB))
    val storage: HistoryStorage = new HistoryStorage(versionalStorage)

    val history: EncryHistory = (settingsEncry.node.stateMode.isDigest, settingsEncry.node.verifyTransactions) match {
      case (true, true) =>
        new EncryHistory with ADStateProofProcessor with BlockPayloadProcessor {
          override protected val settings: EncryAppSettings = settingsEncry
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
          override protected val auxHistory: Boolean = true
        }
      case (false, true) =>
        new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
          override protected val settings: EncryAppSettings = settingsEncry
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
          override protected val auxHistory: Boolean = true
        }
      case (true, false) =>
        new EncryHistory with ADStateProofProcessor with EmptyBlockPayloadProcessor {
          override protected val settings: EncryAppSettings = settingsEncry
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
          override protected val auxHistory: Boolean = true
        }
      case m => throw new Error(s"Unsupported settings ADState=:${m._1}, verifyTransactions=:${m._2}, ")
    }
    history
  }

  case class ReportModifierValid(mod: EncryPersistentModifier)
  case class ReportModifierInvalid(mod: EncryPersistentModifier, progressInfo: ProgressInfo[EncryPersistentModifier])
  case class Append(mod: EncryPersistentModifier)
  case class AuxHistoryChanged(history: EncryHistory)
}
