package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.view.fastSync.SnapshotHolder.{
  SnapshotChunk,
  SnapshotChunkSerializer,
  SnapshotManifest,
  SnapshotManifestSerializer
}
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{ EncryAppSettings, LevelDBSettings }
import encry.storage.VersionalStorage
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import encry.view.state.avlTree.{ InternalNode, Node, NodeSerilalizer, ShadowNode }
import org.encryfoundation.common.utils.Algos
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }
import org.iq80.leveldb.{ DB, Options }
import scorex.crypto.hash.Digest32
import scala.language.postfixOps
import scala.util.Try

final case class SnapshotProcessor(settings: EncryAppSettings, storage: VersionalStorage)
    extends StrictLogging
    with SnapshotProcessorStorageAPI
    with AutoCloseable {

  def processNewSnapshot(state: UtxoState, block: Block): SnapshotProcessor = {
    val potentialManifestId: Digest32 = Algos.hash(state.tree.rootHash ++ block.id)
    val manifestIds: Seq[Array[Byte]] = potentialManifestsIds
    val toApply = manifestIds
      .find(_.sameElements(potentialManifestId))
      .flatMap(bytes => manifestById(StorageKey @@ bytes)) match {
      case Some(elem) => updateBestPotentialSnapshot(elem)
      case None       => createNewSnapshot(state, block, manifestIds: Seq[Array[Byte]])
    }
    if (toApply.nonEmpty) {
      logger.info(s"A new snapshot created successfully. Insertion started.")
      storage.insert(StorageVersion @@ Random.randomBytes(), toApply, List empty)
    } else logger.info(s"The new snapshot didn't create after processing.")
    logger.info(
      s"Best potential manifest after processing snapshot info is ${bestPotentialManifest
        .map(e => Algos.encode(e.manifestId))}. Actual manifest is ${actualManifest.map(e => Algos.encode(e.manifestId))}. " +
        s"Blocks height is ${block.header.height}, id is ${block.encodedId}."
    )
    this
  }

  def processNewBlock(block: Block): SnapshotProcessor = {
    val condition
      : Int = (block.header.height - settings.levelDB.maxVersions) % settings.snapshotSettings.newSnapshotCreationHeight
    logger.info(s"condition = $condition")
    val (toDelete, toInsertNew) =
      if (condition == 0) updateActualSnapshot()
      else List.empty -> List.empty
    if (toDelete.nonEmpty || toInsertNew.nonEmpty) {
      logger.info(
        s"Actual manifest updated. Removed all unnecessary chunks|manifests." +
          s" Block height is ${block.header.height}, id ${block.encodedId}."
      )
      storage.insert(StorageVersion @@ Random.randomBytes(), toInsertNew, toDelete.map(StorageKey @@ _))
      //updateChunksManifestId(actualManifest)
    } else
      logger.info(
        s"Didn't need to update actual manifest. Block height is ${block.header.height}, id is ${block.encodedId}."
      )
    logger.info(
      s"Best potential manifest after processing new block is ${bestPotentialManifest.map(
        e => Algos.encode(e.manifestId)
      )}. Actual manifest is ${actualManifest
        .map(e => Algos.encode(e.manifestId))}. Block height is ${block.header.height}, id is ${block.encodedId}."
    )
    this
  }

  def getChunkById(chunkId: Array[Byte]): Option[SnapshotChunkMessage] =
    storage.get(StorageKey @@ chunkId).flatMap(e => Try(SnapshotChunkMessage.parseFrom(e)).toOption)

  private def updateBestPotentialSnapshot(
    elem: SnapshotManifest
  ): List[(StorageKey, StorageValue)] =
    if (!bestPotentialManifest.exists(_.manifestId.sameElements(elem.manifestId))) {
      val updatedManifest = BestPotentialManifestKey -> StorageValue @@ SnapshotManifestSerializer
        .toProto(elem)
        .toByteArray
      List(updatedManifest)
    } else List.empty

  private def createNewSnapshot(
    state: UtxoState,
    block: Block,
    manifestIds: Seq[Array[Byte]]
  ): List[(StorageKey, StorageValue)] = {
    val rawSubtrees: List[List[Node[StorageKey, StorageValue]]] = state.tree.getChunks(state.tree.rootNode, 1)
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
    val serializedManifest: StorageValue = StorageValue @@ SnapshotManifestSerializer.toProto(manifest).toByteArray
    val manifestToDB: (StorageKey, StorageValue) =
      StorageKey @@ manifest.manifestId -> StorageValue @@ serializedManifest
    val newBestPotentialManifest: (StorageKey, StorageValue) = BestPotentialManifestKey -> serializedManifest
    val updateList: (StorageKey, StorageValue) =
      PotentialManifestsIdsKey -> StorageValue @@ (manifest.manifestId :: manifestIds.toList).flatten.toArray
    newBestPotentialManifest :: manifestToDB :: updateList :: snapshotToDB
  }

  private def updateActualSnapshot(): (List[Array[Byte]], List[(StorageKey, StorageValue)]) = {
    //new actual manifest bytes
    val newActualManifestBytes: Array[Byte] =
      manifestBytesById(BestPotentialManifestKey).getOrElse(Array.emptyByteArray)
    //new actual manifest
    val bestPotentialManifestOpt: Option[SnapshotManifest] =
      SnapshotManifestSerializer.fromProto(SnapshotManifestProtoMessage.parseFrom(newActualManifestBytes)).toOption
    //current actual manifest
    val actualManifestOpt: Option[SnapshotManifest] = actualManifest
    //manifests ids to remove excluding new actual without current actual
    val potentialManifestsExcludingBestPotential: Seq[Array[Byte]] = bestPotentialManifestOpt
      .fold(potentialManifestsIds)(
        elem => potentialManifestsIds.filterNot(_.sameElements(elem.manifestId))
      )
    //manifests ids to remove including actual
    val toDeleteManifests: Seq[Array[Byte]] = actualManifestOpt
      .fold(potentialManifestsExcludingBestPotential)(potentialManifestsExcludingBestPotential :+ _.manifestId)
    //chunks ids connected with new actual manifest
    val newActualChunks: Set[ByteArrayWrapper] = bestPotentialManifestOpt
      .map(_.chunksKeys)
      .getOrElse(List.empty)
      .map(ByteArrayWrapper(_))
      .toSet
    //chunks keys to delete
    val keysToDeleteChunks: List[Array[Byte]] = toDeleteManifests.flatMap { manifestId =>
      manifestById(StorageKey @@ manifestId)
        .map(_.chunksKeys.map(ByteArrayWrapper(_)).toSet)
        .getOrElse(Set.empty)
        .diff(newActualChunks)
    }.distinct.map(_.data).toList

    val updateNewActualManifest: (StorageKey, StorageValue) =
      ActualManifestKey -> StorageValue @@ newActualManifestBytes

    (PotentialManifestsIdsKey :: BestPotentialManifestKey :: keysToDeleteChunks ::: toDeleteManifests.toList) -> List(
      updateNewActualManifest
    )
  }

  override def close(): Unit = storage.close()
}

object SnapshotProcessor extends StrictLogging {

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
