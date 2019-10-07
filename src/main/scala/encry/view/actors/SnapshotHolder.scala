package encry.view.actors

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.view.actors.SnapshotHolder._
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import encry.view.state.avlTree.utils.implicits.Instances._
import cats.syntax.either._
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}

class SnapshotHolder(storage: VersionalStorage) extends Actor with StrictLogging with AutoCloseable {

  var manifestList: List[(SnapshotManifest, List[StorageKey])] = List.empty
  var prevHeight: Int = 0

  // networkController ! RegisterMessagesHandler(smth, self)

  override def receive: Receive = {
    case UpdateSnapshot(block, state) =>
      logger.info(s"Starting snapshot updating. New best block is: ${block.encodedId} at height ${block.header.height}.")
      val snapshotInfo: (SnapshotManifest, List[SnapshotChunk]) = state.tree.initializeSnapshotData(block)
      val snapshotToDB: Seq[(StorageKey, StorageValue)] = snapshotInfo._2.map { elem =>
        val bytes: Array[Byte] = SnapshotChunkSerializer.toProto(elem).toByteArray
        StorageKey @@ Algos.hash(bytes) -> StorageValue @@ bytes
      }
      if (block.header.height - 1100 == prevHeight) {
        logger.info(s"Update current manifests at height ${block.header.height}.")
        val toDelete: List[StorageKey] = manifestList.flatMap(_._2)
        manifestList = List(snapshotInfo._1 -> snapshotToDB.map(_._1).toList)
        prevHeight = block.header.height
        storage.insert(
          StorageVersion @@ snapshotInfo._1.ManifestId,
          snapshotToDB.toList,
          toDelete
        )
      } else {
        manifestList = (snapshotInfo._1 -> snapshotToDB.map(_._1).toList) :: manifestList
        storage.insert(
          StorageVersion @@ snapshotInfo._1.ManifestId,
          snapshotToDB.toList,
          List.empty
        )
      }
    case RequestManifest(remote) =>
      logger.info(s"Got request from $remote.")
    // remote ! ResponseManifest(lastManifest)
    case RequestSnapshot(remote) =>
      logger.info(s"Got request for snapshot from $remote.")
  }

  override def close(): Unit = storage.close()
}

object SnapshotHolder {

  final case class SnapshotManifest(bestBlockId: ModifierId,
                                    rootHash: Array[Byte],
                                    rootNodeBytes: NodeProtoMsg,
                                    stateChunksNumber: Long) {
    val ManifestId: Array[Byte] = Algos.hash(
      Longs.toByteArray(stateChunksNumber) ++ bestBlockId ++ rootHash ++ rootNodeBytes.toByteArray
    )
  }

  final case class SnapshotChunk(nodesList: List[NodeProtoMsg], manifestId: Array[Byte] = Array.emptyByteArray)

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class RequestManifest(remote: ActorRef) extends AnyVal

  final case class ResponseManifest(manifest: SnapshotManifest) extends AnyVal

  final case class RequestSnapshot(remote: ActorRef) extends AnyVal

}

object SnapshotChunkSerializer {

  def toProto(chunk: SnapshotChunk): SnapshotChunkMessage = SnapshotChunkMessage()
    .withChunks(chunk.nodesList)
    .withManifestId(ByteString.copyFrom(chunk.manifestId))

  def fromProto(chunk: SnapshotChunkMessage): Either[Throwable, SnapshotChunk] = Either.catchNonFatal(
    SnapshotChunk(chunk.chunks.toList, chunk.manifestId.toByteArray)
  )
}

object SnapshotManifestSerializer {

  def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage = SnapshotManifestProtoMessage()
    .withBestBlockId(ByteString.copyFrom(manifest.bestBlockId))
    .withRootHash(ByteString.copyFrom(manifest.rootHash))
    .withRootNodeBytes(manifest.rootNodeBytes)
    .withStateChunksNumber(manifest.stateChunksNumber)

  def fromProto(manifest: SnapshotManifestProtoMessage): Either[Throwable, SnapshotManifest] = Either.catchNonFatal(
    SnapshotManifest(
      ModifierId @@ manifest.bestBlockId.toByteArray,
      manifest.rootHash.toByteArray,
      manifest.rootNodeBytes.get,
      manifest.stateChunksNumber
    )
  )
}