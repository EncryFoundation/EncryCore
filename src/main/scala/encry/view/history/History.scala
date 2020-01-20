package encry.view.history

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.settings._
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBHistoryWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.NetworkTimeProvider
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.iq80.leveldb.Options
import cats.syntax.either._
import supertagged.@@

/**
  * History implementation. It is processing persistent modifiers generated locally or received from the network.
  **/
trait History extends HistoryModifiersValidator with AutoCloseable {


}

object History extends StrictLogging {

  def getHistoryIndexDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/history/index")
    dir.mkdirs()
    dir
  }

  def getHistoryObjectsDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/history/objects")
    dir.mkdirs()
    dir
  }

  def readOrGenerate(settingsEncry: EncryAppSettings, ntp: NetworkTimeProvider): History = {
    val historyIndexDir: File = getHistoryIndexDir(settingsEncry)
    //Check what kind of storage in settings:
    val vldbInit = settingsEncry.storage.history match {
      case VersionalStorage.IODB =>
        logger.info("Init history with iodb storage")
        val historyObjectsDir: File = getHistoryObjectsDir(settingsEncry)
        val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
        val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
        IODBHistoryWrapper(indexStore, objectsStore)
      case VersionalStorage.LevelDB =>
        logger.info("Init history with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(historyIndexDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settingsEncry.levelDB))
    }
    if (settingsEncry.snapshotSettings.enableFastSynchronization && !settingsEncry.node.offlineGeneration)
      new History with HistoryHeadersProcessor with FastSyncProcessor {
        override val settings: EncryAppSettings = settingsEncry
        override var isFullChainSynced: Boolean = settings.node.offlineGeneration
        override val historyStorage: HistoryStorage = HistoryStorage(vldbInit)
        override val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)
      }
    else
      new History with HistoryHeadersProcessor with HistoryPayloadsProcessor {
        override val settings: EncryAppSettings = settingsEncry
        override var isFullChainSynced: Boolean = settings.node.offlineGeneration
        override val historyStorage: HistoryStorage = HistoryStorage(vldbInit)
        override val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)
      }

  }
}