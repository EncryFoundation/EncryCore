package encry.view.fast.sync

import java.io.File
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
import encry.view.state.avlTree.{ AvlTree, InternalNode, LeafNode, Node, NodeSerilalizer, ShadowNode }
import org.encryfoundation.common.utils.Algos
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }
import org.iq80.leveldb.{ DB, Options }
import cats.syntax.either._
import scala.language.postfixOps
import com.google.common.primitives.Ints
import encry.view.fast.sync.FastSyncExceptions.{
  ApplicableChunkIsAbsent,
  BestHeaderAtHeightIsAbsent,
  ChunkApplyError,
  ChunkValidationError,
  EmptyHeightKey,
  EmptyRootNodeError,
  FastSyncException,
  InconsistentChunkId,
  InitializeHeightAndRootKeysException,
  ProcessNewBlockError,
  ProcessNewSnapshotError,
  UtxoCreationError
}
import encry.view.history.History
import org.encryfoundation.common.utils.TaggedTypes.Height
import scala.collection.immutable.{ HashMap, HashSet }

final case class SnapshotProcessor(settings: EncryAppSettings,
                                   storage: VersionalStorage,
                                   applicableChunks: HashSet[ByteArrayWrapper],
                                   chunksCache: HashMap[ByteArrayWrapper, SnapshotChunk])
    extends StrictLogging
    with SnapshotProcessorStorageAPI
    with AutoCloseable {

  def updateCache(chunk: SnapshotChunk): SnapshotProcessor =
    this.copy(chunksCache = chunksCache.updated(ByteArrayWrapper(chunk.id), chunk))

  def initializeApplicableChunksCache(history: History, height: Int): Either[FastSyncException, SnapshotProcessor] =
    for {
      stateRoot <- Either.fromOption(
                    history.getBestHeaderAtHeight(height).map(_.stateRoot),
                    BestHeaderAtHeightIsAbsent(s"There is no best header at required height $height")
                  )
      processor: SnapshotProcessor = this.copy(applicableChunks = HashSet(ByteArrayWrapper(stateRoot)))
      resultedProcessor <- processor.initializeHeightAndRootKeys(stateRoot, height) match {
                            case Left(error) =>
                              InitializeHeightAndRootKeysException(error.getMessage).asLeft[SnapshotProcessor]
                            case Right(newProcessor) => newProcessor.asRight[FastSyncException]
                          }
    } yield resultedProcessor

  private def initializeHeightAndRootKeys(rootNodeId: Array[Byte], height: Int): Either[Throwable, SnapshotProcessor] =
    Either.catchNonFatal {
      storage.insert(
        StorageVersion @@ Random.randomBytes(),
        AvlTree.rootNodeKey       -> StorageValue @@ rootNodeId ::
          UtxoState.bestHeightKey -> StorageValue @@ Ints.toByteArray(height) :: Nil
      )
      this
    }

  def reInitStorage: SnapshotProcessor =
    try {
      storage.close()
      val dir: File = SnapshotProcessor.getDirProcessSnapshots(settings)
      import org.apache.commons.io.FileUtils
      FileUtils.deleteDirectory(dir)
      SnapshotProcessor.initialize(settings)
    } catch {
      case _: Throwable => sys.exit(9999)
    }

  private def flatten(node: Node[StorageKey, StorageValue]): List[Node[StorageKey, StorageValue]] = node match {
    case shadowNode: ShadowNode[StorageKey, StorageValue] => shadowNode :: Nil
    case leaf: LeafNode[StorageKey, StorageValue]         => leaf :: Nil
    case internalNode: InternalNode[StorageKey, StorageValue] =>
      internalNode ::
        internalNode.leftChild.map(flatten).getOrElse(List.empty[Node[StorageKey, StorageValue]]) :::
        internalNode.rightChild.map(flatten).getOrElse(List.empty[Node[StorageKey, StorageValue]])
  }

  def applyChunk(chunk: SnapshotChunk): Either[ChunkApplyError, SnapshotProcessor] = {
    val kSerializer: Serializer[StorageKey]         = implicitly[Serializer[StorageKey]]
    val vSerializer: Serializer[StorageValue]       = implicitly[Serializer[StorageValue]]
    val nodes: List[Node[StorageKey, StorageValue]] = flatten(chunk.node)
    val toApplicable                                = nodes.collect { case node: ShadowNode[StorageKey, StorageValue] => node }
    val toStorage = nodes.collect {
      case leaf: LeafNode[StorageKey, StorageValue]         => leaf
      case internal: InternalNode[StorageKey, StorageValue] => internal
    }
    val nodesToInsert: List[(StorageKey, StorageValue)] = toStorage.flatMap { node =>
      val fullData: (StorageKey, StorageValue) =
        StorageKey @@ Algos.hash(kSerializer.toBytes(node.key).reverse) -> StorageValue @@ vSerializer.toBytes(
          node.value
        )
      val shadowData: (StorageKey, StorageValue) =
        StorageKey @@ node.hash -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(node)) //todo probably probably probably
      fullData :: shadowData :: Nil
    }
    Either.catchNonFatal(
      storage.insert(StorageVersion @@ Random.randomBytes(), nodesToInsert, List.empty)
    ) match {
      case Right(_) =>
        logger.info(s"Chunk ${Algos.encode(chunk.id)} applied successfully.")
        val newApplicableChunk = (applicableChunks -- toStorage.map(node => ByteArrayWrapper(node.hash))) ++
          toApplicable.map(node => ByteArrayWrapper(node.hash))
        this.copy(applicableChunks = newApplicableChunk).asRight[ChunkApplyError]
      case Left(exception) => ChunkApplyError(exception.getMessage).asLeft[SnapshotProcessor]
    }
  }

  def validateChunkId(chunk: SnapshotChunk): Either[ChunkValidationError, SnapshotChunk] =
    if (chunk.node.hash.sameElements(chunk.id)) chunk.asRight[ChunkValidationError]
    else
      InconsistentChunkId(
        s"Node hash:(${Algos.encode(chunk.node.hash)}) doesn't equal to chunk id:(${Algos.encode(chunk.id)})"
      ).asLeft[SnapshotChunk]

  private def getNextApplicableChunk: Either[FastSyncException, (SnapshotChunk, SnapshotProcessor)] =
    for {
      idAndChunk <- Either.fromOption(chunksCache.find { case (id, _) => applicableChunks.contains(id) },
                                      ApplicableChunkIsAbsent("There are no applicable chunks in cache", this))
      (id: ByteArrayWrapper, chunk: SnapshotChunk)             = idAndChunk
      newChunksCache: HashMap[ByteArrayWrapper, SnapshotChunk] = chunksCache - id
    } yield (chunk, this.copy(chunksCache = newChunksCache))

  def processNextApplicableChunk(snapshotProcessor: SnapshotProcessor): Either[FastSyncException, SnapshotProcessor] =
    for {
      chunkAndProcessor  <- snapshotProcessor.getNextApplicableChunk
      (chunk, processor) = chunkAndProcessor
      resultedProcessor  <- processor.applyChunk(chunk)
      processor          <- resultedProcessor.processNextApplicableChunk(resultedProcessor)
    } yield processor

  def assembleUTXOState: Either[UtxoCreationError, UtxoState] =
    for {
      rootNode <- getRootNode
      height   <- getHeight
      avlTree  = new AvlTree[StorageKey, StorageValue](rootNode, storage)
    } yield UtxoState(avlTree, height, settings.constants)

  private def getHeight: Either[EmptyHeightKey, Height] =
    Either.fromOption(storage.get(UtxoState.bestHeightKey).map(Height @@ Ints.fromByteArray(_)),
                      EmptyHeightKey("bestHeightKey is empty"))

  private def getRootNodeId: Either[EmptyRootNodeError, StorageKey] =
    Either.fromOption(
      storage.get(AvlTree.rootNodeKey).map(StorageKey !@@ _),
      EmptyRootNodeError("Root node key doesn't exist")
    )

  private def getNode(nodeId: Array[Byte]): Either[EmptyRootNodeError, Node[StorageKey, StorageValue]] =
    Either.fromOption(
      storage.get(StorageKey @@ nodeId).map(NodeSerilalizer.fromBytes[StorageKey, StorageValue](_)),
      EmptyRootNodeError(s"Node with id ${Algos.encode(nodeId)} doesn't exist")
    )

  private def getRootNode: Either[EmptyRootNodeError, Node[StorageKey, StorageValue]] =
    for {
      rootNodeId <- getRootNodeId
      node       <- getNode(rootNodeId)
    } yield node

  def processNewBlock(block: Block, history: History): Either[ProcessNewBlockError, SnapshotProcessor] = {
    logger.info(
      s"Start updating actual manifest to new one at height " +
        s"${block.header.height} with block id ${block.encodedId}."
    )
    updateActualSnapshot(history, block.header.height - settings.levelDB.maxVersions)
  }

  def createNewSnapshot(
    id: Array[Byte],
    manifestIds: Seq[Array[Byte]],
    newChunks: List[SnapshotChunk]
  ): Either[ProcessNewSnapshotError, SnapshotProcessor] = {
    //todo add only exists chunks
    val manifest: SnapshotManifest = SnapshotManifest(id, newChunks.map(_.id))
    val snapshotToDB: List[(StorageKey, StorageValue)] = newChunks.map { elem =>
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
    Either.catchNonFatal(storage.insert(StorageVersion @@ Random.randomBytes(), toApply, List.empty)) match {
      case Left(value) => ProcessNewSnapshotError(value.getMessage).asLeft[SnapshotProcessor]
      case Right(_)    => this.asRight[ProcessNewSnapshotError]
    }
  }

  private def updateActualSnapshot(history: History, height: Int): Either[ProcessNewBlockError, SnapshotProcessor] =
    for {
      bestManifestId <- Either.fromOption(
                         history.getBestHeaderAtHeight(height).map(header => Algos.hash(header.stateRoot ++ header.id)),
                         ProcessNewBlockError(s"There is no best header at height $height")
                       )
      processor <- {
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
        Either.catchNonFatal(
          storage.insert(StorageVersion @@ Random.randomBytes(), List(toApply), toDelete.map(StorageKey @@ _))
        ) match {
          case Left(error) => ProcessNewBlockError(error.getMessage).asLeft[SnapshotProcessor]
          case Right(_)    => this.asRight[ProcessNewBlockError]
        }
      }
    } yield processor

  override def close(): Unit = storage.close()
}

object SnapshotProcessor extends StrictLogging {

  def initialize(settings: EncryAppSettings): SnapshotProcessor = create(settings, getDirProcessSnapshots(settings))

  def getDirProcessSnapshots(settings: EncryAppSettings): File = new File(s"${settings.directory}/snapshots")

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
    new SnapshotProcessor(settings, storage, HashSet.empty, HashMap.empty)
  }
}
