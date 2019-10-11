package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.view.fastSync.SnapshotHolder.{ SnapshotChunk, SnapshotChunkSerializer, SnapshotManifest }
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{ EncryAppSettings, LevelDBSettings }
import encry.storage.VersionalStorage
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import org.encryfoundation.common.utils.Algos
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }
import org.iq80.leveldb.{ DB, Options }
import scala.language.postfixOps
import scala.util.Try

final case class SnapshotProcessor(actualManifest: Option[SnapshotManifest],
                                   potentialManifests: List[SnapshotManifest],
                                   bestPotentialManifest: Option[SnapshotManifest],
                                   settings: EncryAppSettings,
                                   storage: VersionalStorage)
    extends StrictLogging
    with AutoCloseable {

  def processNewSnapshot(state: UtxoState, block: Block): SnapshotProcessor = {
    val (processor, toApply) = potentialManifests find (
      manifest => (manifest.bestBlockId sameElements block.id) && (manifest.rootHash sameElements state.tree.rootHash)
    ) match {
      case Some(elem) => updateBestPotentialSnapshot(elem)
      case None       => createNewSnapshot(state, block)
    }
    if (toApply nonEmpty) {
      logger.info(s"New snapshot has created successfully. Insert has started.")
      storage.insert(StorageVersion @@ Random.randomBytes(), toApply, List empty)
    } else logger.info(s"New snapshot has not created after processing.")
    logger.info(s"Best potential manifest after processing snapshot info is ${processor.bestPotentialManifest map (
      e => Algos.encode(e.rootHash)
    )}. Actual manifest is ${processor.actualManifest map (e => Algos.encode(e.rootHash))}.")
    processor
  }

  def processNewBlock(block: Block): SnapshotProcessor = {
    val condition: Int = (block.header.height - settings.levelDB.maxVersions) % 200
    logger.info(s"Condition $condition.")
    val (processor, toDelete) =
      if (condition == 0) updateActualSnapshot()
      else this -> List.empty[StorageKey]
    if (toDelete nonEmpty) {
      logger.info(s"New actual manifest has created.")
      storage.insert(StorageVersion @@ Random.randomBytes(), List.empty, toDelete)
    } else logger.info("Didn't need to update actual snapshot.")
    logger.info(s"Best potential manifest after processing new block is ${processor.bestPotentialManifest map (
      e => Algos.encode(e.rootHash)
    )}. Actual manifest is ${processor.actualManifest map (e => Algos.encode(e.rootHash))}.")
    processor
  }

  def getChunkById(chunkId: Array[Byte]): Option[SnapshotChunkMessage] =
    storage.get(StorageKey @@ chunkId).flatMap(e => Try(SnapshotChunkMessage.parseFrom(e)).toOption)

  @deprecated def restoreActualChunks: List[Array[Byte]] = {
    logger.info(s"Restoring actual chunks.")
    actualManifest.map(_.chunksKeys.map(StorageKey @@ _)).getOrElse(List.empty).flatMap(storage.get)
  }

  private def updateBestPotentialSnapshot(
    elem: SnapshotManifest
  ): (SnapshotProcessor, List[(StorageKey, StorageValue)]) =
    this.copy(bestPotentialManifest = elem.some) -> List.empty[(StorageKey, StorageValue)]

  private def createNewSnapshot(state: UtxoState,
                                block: Block): (SnapshotProcessor, List[(StorageKey, StorageValue)]) = {
    val (manifest: SnapshotManifest, chunks: List[SnapshotChunk]) = state.tree.initializeSnapshotData(block)
    val snapshotToDB: List[(StorageKey, StorageValue)] = chunks.map { elem =>
      val bytes: Array[Byte] = SnapshotChunkSerializer.toProto(elem).toByteArray
      StorageKey @@ elem.id -> StorageValue @@ bytes
    }
    this.copy(
      potentialManifests = manifest :: potentialManifests,
      bestPotentialManifest = manifest.some
    ) -> snapshotToDB
  }

  private def updateActualSnapshot(): (SnapshotProcessor, List[StorageKey]) = {

    val manifestsToDelete: List[StorageKey] = potentialManifests.foldLeft(List.empty[StorageKey]) {
      case (toDelete, manifest) if bestPotentialManifest.exists(_.ManifestId sameElements manifest.ManifestId) =>
        toDelete
      case (toDelete, manifest) => toDelete ::: manifest.chunksKeys.map(StorageKey @@ _)
    }
    val newActual = bestPotentialManifest.map(_.chunksKeys).getOrElse(List.empty).map(ByteArrayWrapper(_))
    val resultToDelete = manifestsToDelete
      .map(ByteArrayWrapper(_))
      .filterNot(newActual.contains(_))
      .map(e => StorageKey @@ e.data)
    this.copy(
      actualManifest = bestPotentialManifest,
      potentialManifests = List.empty,
      bestPotentialManifest = none
    ) -> resultToDelete
  }

  override def close(): Unit = storage.close()
}

object SnapshotProcessor extends StrictLogging {

  def initialize(settings: EncryAppSettings): SnapshotProcessor = create(settings, getDir(settings))

  def getDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/snapshots")

  def create(settings: EncryAppSettings, snapshotsDir: File): SnapshotProcessor = {
    snapshotsDir.mkdirs()
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
      none[SnapshotManifest],
      List.empty[SnapshotManifest],
      none[SnapshotManifest],
      settings,
      storage
    )
  }
}
