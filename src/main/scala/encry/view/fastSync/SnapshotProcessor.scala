package encry.view.fastSync

import java.io.File
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.fastSync.SnapshotHolder.{SnapshotChunk, SnapshotChunkSerializer, SnapshotManifest}
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{EncryAppSettings, LevelDBSettings}
import encry.storage.VersionalStorage
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import org.encryfoundation.common.utils.Algos
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.LSMStore
import org.iq80.leveldb.{DB, Options}
import scala.language.postfixOps

final case class SnapshotProcessor(actualManifest: Option[(SnapshotManifest, List[StorageKey])],
                                   potentialManifests: List[(SnapshotManifest, List[StorageKey])],
                                   bestPotentialManifest: Option[(SnapshotManifest, List[StorageKey])],
                                   settings: EncryAppSettings,
                                   storage: VersionalStorage) extends StrictLogging with AutoCloseable {

  def processNewSnapshot(state: UtxoState, block: Block): SnapshotProcessor = {
    val (processor, toDelete) = potentialManifests find { case (manifest, _) =>
      (manifest.bestBlockId sameElements block.id) && (manifest.rootHash sameElements state.tree.rootHash)
    } match {
      case Some(elem) => updateBestPotentialSnapshot(elem)
      case None => createNewSnapshot(state, block)
    }
    if (toDelete nonEmpty) {
      logger.info(s"New snapshot has created successfully. Insert has started.")
      storage.insert(StorageVersion @@ Random.randomBytes(), toDelete, List empty)
    } else logger.info(s"New snapshot has not created after processing.")
    logger.info(s"Best potential manifest after processing snapshot info is ${
      processor.bestPotentialManifest map (e => Algos.encode(e._1.rootHash))
    }. Actual manifest is ${
      processor.actualManifest map (e => Algos.encode(e._1.rootHash))
    }.")
    processor
  }

  def processNewBlock(block: Block): SnapshotProcessor = {
    val condition: Int = (block.header.height - settings.levelDB.maxVersions) % 1000
    logger.info(s"Condition $condition.")
    val (processor, toDelete) =
      if (condition == 0) updateActualSnapshot()
      else this -> List.empty[StorageKey]
    if (toDelete nonEmpty) {
      logger.info(s"New actual manifest has created.")
      storage.insert(StorageVersion @@ Random.randomBytes(), List.empty, toDelete)
    } else logger.info("Didn't need to update actual snapshot.")
    logger.info(s"Best potential manifest after processing new block is ${
      processor.bestPotentialManifest map (e => Algos.encode(e._1.rootHash))
    }. Actual manifest is ${
      processor.actualManifest map (e => Algos.encode(e._1.rootHash))
    }.")
    processor
  }

  def restoreActualChunks: List[Array[Byte]] = {
    logger.info(s"Restoring actual chunks.")
    actualManifest.map(_._2).getOrElse(List.empty).flatMap(storage.get)
  }

  private def updateBestPotentialSnapshot(elem: (SnapshotManifest, List[StorageKey])): (SnapshotProcessor, List[(StorageKey, StorageValue)]) =
    this.copy(bestPotentialManifest = elem.some) -> List.empty[(StorageKey, StorageValue)]

  private def createNewSnapshot(state: UtxoState, block: Block): (SnapshotProcessor, List[(StorageKey, StorageValue)]) = {
    val (manifest: SnapshotManifest, chunks: List[SnapshotChunk]) = state.tree.initializeSnapshotData(block)
    val snapshotToDB: List[(StorageKey, StorageValue)] = chunks.map { elem =>
      val bytes: Array[Byte] = SnapshotChunkSerializer.toProto(elem).toByteArray
      StorageKey @@ Algos.hash(bytes) -> StorageValue @@ bytes
    }
    val infoForUpdate: (SnapshotManifest, List[StorageKey]) = manifest -> snapshotToDB.map(_._1)
    this.copy(
      potentialManifests = infoForUpdate :: potentialManifests,
      bestPotentialManifest = infoForUpdate.some
    ) -> snapshotToDB
  }

  private def updateActualSnapshot(): (SnapshotProcessor, List[StorageKey]) = {
    val newActualSnapshot: Option[(SnapshotManifest, List[StorageKey])] = bestPotentialManifest
    val manifestsToDelete: List[StorageKey] = potentialManifests.foldLeft(List.empty[StorageKey]) {
      case (toDelete, (manifest, _)) if newActualSnapshot.exists(_._1.ManifestId sameElements manifest.ManifestId) =>
        toDelete
      case (toDelete, (_, keys)) =>
        toDelete ::: keys
    }
    this.copy(
      actualManifest = newActualSnapshot,
      potentialManifests = List.empty,
      bestPotentialManifest = none
    ) -> manifestsToDelete
  }

  override def close(): Unit = storage.close()
}

object SnapshotProcessor extends StrictLogging {

  def initialize(settings: EncryAppSettings): SnapshotProcessor = create(settings, getDir(settings))

  def getDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/snapshots")

  def create(settings: EncryAppSettings, snapshotsDir: File): SnapshotProcessor = {
    val storage: VersionalStorage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init snapshots holder with iodb storage")
        IODBWrapper(new LSMStore(snapshotsDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init snapshots holder with levelDB storage")
        val levelDBInit: DB = LevelDbFactory.factory.open(snapshotsDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300), keySize = 32))
    }
    new SnapshotProcessor(
      none[(SnapshotManifest, List[StorageKey])],
      List.empty[(SnapshotManifest, List[StorageKey])],
      none[(SnapshotManifest, List[StorageKey])],
      settings,
      storage
    )
  }
}