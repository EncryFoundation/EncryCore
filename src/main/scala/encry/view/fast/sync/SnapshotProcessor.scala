package encry.view.fast.sync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{ EncryAppSettings, LevelDBSettings }
import encry.storage.VersionalStorage
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import encry.view.fast.sync.SnapshotHolder.{
  SnapshotChunk,
  SnapshotChunkSerializer,
  SnapshotManifest,
  SnapshotManifestSerializer
}
import encry.view.state.avlTree.{ InternalNode, Node, NodeSerilalizer, ShadowNode }
import org.encryfoundation.common.utils.Algos
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }
import org.iq80.leveldb.{ DB, Options }
import scorex.crypto.hash.Digest32
import scala.language.postfixOps
import cats.syntax.either._
import encry.view.fast.sync.SnapshotProcessor.{ ProcessNewBlockError, ProcessNewSnapshotError }
import encry.view.history.History
import scala.util.Try

final case class SnapshotProcessor(settings: EncryAppSettings, storage: VersionalStorage)
    extends StrictLogging
    with SnapshotProcessorStorageAPI
    with AutoCloseable {

  def processNewSnapshot(state: UtxoState, block: Block): Either[ProcessNewSnapshotError, SnapshotProcessor] = {
    val potentialManifestId: Digest32 = Algos.hash(state.tree.rootHash ++ block.id)
    logger.info(
      s"Started processing new snapshot with block ${block.encodedId} at height ${block.header.height}" +
        s" and manifest id ${Algos.encode(potentialManifestId)}."
    )
    val manifestIds: Seq[Array[Byte]] = potentialManifestsIds
    if (!potentialManifestsIds.exists(_.sameElements(potentialManifestId))) {
      logger.info(s"Need to create new best potential snapshot.")
      createNewSnapshot(state, block, manifestIds)
    } else {
      logger.info(s"This snapshot already exists.")
      this.asRight[ProcessNewSnapshotError]
    }
  }

  def processNewBlock(block: Block, history: History): Either[ProcessNewBlockError, SnapshotProcessor] = {
    val condition: Int =
      (block.header.height - settings.levelDB.maxVersions) % settings.snapshotSettings.newSnapshotCreationHeight
    logger.info(s"condition = $condition")
    if (condition == 0) {
      logger.info(s"Start updating actual manifest to new one at height " +
        s"${block.header.height} with block id ${block.encodedId}.")
      updateActualSnapshot(history, block.header.height - settings.levelDB.maxVersions)
    } else {
      logger.info(s"Doesn't need to update actual manifest.")
      this.asRight[ProcessNewBlockError]
    }
  }

  def getChunkById(chunkId: Array[Byte]): Option[SnapshotChunkMessage] =
    storage.get(StorageKey @@ chunkId).flatMap(e => Try(SnapshotChunkMessage.parseFrom(e)).toOption)

  private def createNewSnapshot(
    state: UtxoState,
    block: Block,
    manifestIds: Seq[Array[Byte]]
  ): Either[ProcessNewSnapshotError, SnapshotProcessor] = {
    //todo add only exists chunks
    val rawSubtrees: List[List[Node[StorageKey, StorageValue]]] =
      state.tree.getChunks(state.tree.rootNode, currentChunkHeight = 1)
    val newChunks: List[SnapshotChunk] = rawSubtrees.map { l =>
      val chunkId: Array[Byte] = l.headOption.map(_.hash).getOrElse(Array.emptyByteArray)
      SnapshotChunk(l.map(NodeSerilalizer.toProto[StorageKey, StorageValue](_)), chunkId)
    }
    val manifest: SnapshotManifest = state.tree.rootNode match {
      case i: InternalNode[StorageKey, StorageValue] =>
        SnapshotManifest(Algos.hash(state.tree.rootHash ++ block.id), NodeSerilalizer.toProto(i), newChunks.map(_.id))
      case s: ShadowNode[StorageKey, StorageValue] =>
        SnapshotManifest(Algos.hash(state.tree.rootHash ++ block.id),
                         NodeSerilalizer.toProto(s.restoreFullNode(storage)),
                         newChunks.map(_.id))
    }
    val updatedSubtrees: Option[SnapshotChunk]    = newChunks.headOption.map(node => node.copy(node.nodesList.drop(1)))
    val subtreesWithOutFirst: List[SnapshotChunk] = newChunks.drop(1)
    val chunks: List[SnapshotChunk]               = updatedSubtrees.fold(subtreesWithOutFirst)(_ :: subtreesWithOutFirst)

    val snapshotToDB: List[(StorageKey, StorageValue)] = chunks.map { elem =>
      val bytes: Array[Byte] = SnapshotChunkSerializer.toProto(elem).toByteArray
      StorageKey @@ elem.id -> StorageValue @@ bytes
    }
    val manifestToDB: (StorageKey, StorageValue) =
      StorageKey @@ manifest.manifestId -> StorageValue @@ SnapshotManifestSerializer
        .toProto(manifest)
        .toByteArray
    val updateList: (StorageKey, StorageValue) =
      PotentialManifestsIdsKey -> StorageValue @@ (manifest.manifestId :: manifestIds.toList).flatten.toArray
    val toApply: List[(StorageKey, StorageValue)] = manifestToDB :: updateList :: snapshotToDB
    logger.info(s"A new snapshot created successfully. Insertion started.")
    Either.fromTry(Try(storage.insert(StorageVersion @@ Random.randomBytes(), toApply, List.empty))) match {
      case Left(value) =>
        logger.info(value.getMessage)
        ProcessNewSnapshotError(value.getMessage).asLeft[SnapshotProcessor]
      case Right(_) =>
        this.asRight[ProcessNewSnapshotError]
    }
  }

  private def updateActualSnapshot(history: History, height: Int): Either[ProcessNewBlockError, SnapshotProcessor] =
    Either.fromOption(
      history.getBestHeaderAtHeight(height).map { header =>
        val id: Digest32 = Algos.hash(header.stateRoot ++ header.id)
        logger.info(
          s"Block id at height $height is ${header.encodedId}. State root is ${Algos.encode(header.stateRoot)}" +
            s" Expected manifest id is ${Algos.encode(id)}")
        id
      },
      ProcessNewBlockError(s"There is no best header at height $height")
    ) match {
      case Left(error) =>
        logger.info(error.toString)
        error.asLeft[SnapshotProcessor]
      case Right(bestManifestId) =>
        logger.info(s"Expected manifest id at height $height is ${Algos.encode(bestManifestId)}")
        val manifestIdsToRemove: Seq[Array[Byte]]    = potentialManifestsIds.filterNot(_.sameElements(bestManifestId))
        val manifestsToRemove: Seq[SnapshotManifest] = manifestIdsToRemove.flatMap(l => manifestById(StorageKey @@ l))
        val chunksToRemove: Set[ByteArrayWrapper] =
          manifestsToRemove.flatMap(_.chunksKeys.map(ByteArrayWrapper(_))).toSet
        val newActualManifest: Option[SnapshotManifest] = manifestById(StorageKey @@ bestManifestId)
        val excludedIds: Set[ByteArrayWrapper] =
          newActualManifest.toList.flatMap(_.chunksKeys.map(ByteArrayWrapper(_))).toSet
        val resultedChunksToRemove: List[Array[Byte]] = chunksToRemove.diff(excludedIds).map(_.data).toList
        val toDelete: List[Array[Byte]] =
          PotentialManifestsIdsKey :: manifestIdsToRemove.toList ::: resultedChunksToRemove
        val toApply: (StorageKey, StorageValue) = ActualManifestKey -> StorageValue @@ bestManifestId
        Either.fromTry(
          Try(storage.insert(StorageVersion @@ Random.randomBytes(), List(toApply), toDelete.map(StorageKey @@ _)))
        ) match {
          case Left(error) =>
            logger.info(error.getMessage)
            ProcessNewBlockError(error.getMessage).asLeft[SnapshotProcessor]
          case Right(_) =>
            this.asRight[ProcessNewBlockError]
        }
    }

  override def close(): Unit = storage.close()
}

object SnapshotProcessor extends StrictLogging {

  sealed trait SnapshotProcessorError
  final case class ProcessNewSnapshotError(str: String) extends SnapshotProcessorError
  final case class ProcessNewBlockError(str: String)    extends SnapshotProcessorError

  def initialize(settings: EncryAppSettings): SnapshotProcessor = create(settings, getDir(settings))

  def getDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/snapshots")

  def create(settings: EncryAppSettings, snapshotsDir: File): SnapshotProcessor = {
    snapshotsDir.mkdirs()
    val storage: VersionalStorage = settings.storage.snapshotHolder match {
      case VersionalStorage.IODB =>
        logger.info("Init snapshots holder with iodb storage")
        IODBWrapper(new LSMStore(snapshotsDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init snapshots holder with levelDB storage")
        val levelDBInit: DB = LevelDbFactory.factory.open(snapshotsDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300), keySize = 32))
    }
    new SnapshotProcessor(settings, storage)
  }
}
