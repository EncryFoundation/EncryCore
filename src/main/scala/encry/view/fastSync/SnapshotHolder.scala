package encry.view.fastSync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.Actor
import com.google.common.primitives.{Ints, Longs}
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.fastSync.SnapshotHolder._
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Serializer
import encry.view.state.avlTree.{Node, NodeSerilalizer}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.util.Try

class SnapshotHolder(settings: EncryAppSettings) extends Actor with StrictLogging {

  var snapshotProcessor: SnapshotProcessor = SnapshotProcessor.initialize(settings)

  // networkController ! RegisterMessagesHandler(smth, self)

  override def receive: Receive = {
    case UpdateSnapshot(block, state) =>
      logger.info(s"Snapshot holder got update snapshot message. Potential snapshot processing has started.")
      val newProcessor: SnapshotProcessor = snapshotProcessor.processNewSnapshot(state, block)
      snapshotProcessor = newProcessor

    case SemanticallySuccessfulModifier(block: Block) =>
      logger.info(s"Snapshot holder got semantically successful modifier message. Has started processing it.")
      val newProcessor: SnapshotProcessor = snapshotProcessor.processNewBlock(block)
      snapshotProcessor = newProcessor

    case RequestActualManifest(remote) =>
      logger.info(s"${remote.socketAddress} has asked actual manifest.")
      remote.handlerRef ! ResponseManifest(snapshotProcessor.actualManifest)

    case RequestSnapshot(remote) =>
      logger.info(s"${remote.socketAddress} has asked actual chunks.")
      snapshotProcessor.restoreActualChunks.foreach(bytes => remote.handlerRef ! ResponseSnapshotChunk(bytes))

    case SemanticallySuccessfulModifier(_) => //do nothing
    case nonsense => logger.info(s"Snapshot holder got strange message $nonsense.")
  }

}

object SnapshotHolder {

  final case class SnapshotManifest(bestBlockId: ModifierId,
                                    rootHash: Array[Byte],
                                    rootNodeBytes: NodeProtoMsg,
                                    stateChunksNumber: Long,
                                    bestBlockHeight: Int,
                                    chunksKeys: List[Array[Byte]]) {
    val ManifestId: Array[Byte] = Algos.hash(
      Longs.toByteArray(stateChunksNumber) ++ bestBlockId ++ rootHash ++
        rootNodeBytes.toByteArray ++ Ints.toByteArray(bestBlockHeight) ++ chunksKeys.flatten.toArray[Byte]
    )
  }

  object SnapshotManifestSerializer {

    def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage = SnapshotManifestProtoMessage()
      .withBestBlockId(ByteString.copyFrom(manifest.bestBlockId))
      .withRootHash(ByteString.copyFrom(manifest.rootHash))
      .withRootNodeBytes(manifest.rootNodeBytes)
      .withStateChunksNumber(manifest.stateChunksNumber)
      .withBestBlockHeight(manifest.bestBlockHeight)
      .withChunksIds(manifest.chunksKeys.map(ByteString.copyFrom))


    def fromProto(manifest: SnapshotManifestProtoMessage): Try[SnapshotManifest] = Try(
      SnapshotManifest(
        ModifierId @@ manifest.bestBlockId.toByteArray,
        manifest.rootHash.toByteArray,
        manifest.rootNodeBytes.get,
        manifest.stateChunksNumber,
        manifest.bestBlockHeight,
        manifest.chunksIds.map(_.toByteArray).toList
      )
    )
  }


  final case class SnapshotChunk(nodesList: List[NodeProtoMsg],
                                 manifestId: Array[Byte],
                                 id: Array[Byte])

  object SnapshotChunk {
    def apply[K: Serializer, V: Serializer](list: List[Node[K, V]]): SnapshotChunk = {
      val chunkId: Array[Byte] = Algos.hash(list.flatMap(_.hash).toArray)
      new SnapshotChunk(
        list.map(NodeSerilalizer.toProto(_)),
        Array.emptyByteArray,
        chunkId
      )
    }

    def apply[K: Serializer, V: Serializer](list: List[Node[K, V]], manifestId: Array[Byte]): SnapshotChunk = {
      val chunkId: Array[Byte] = Algos.hash(list.flatMap(_.hash).toArray)
      new SnapshotChunk(
        list.map(NodeSerilalizer.toProto(_)),
        manifestId,
        chunkId
      )
    }
  }

  object SnapshotChunkSerializer {

    import encry.view.state.avlTree.utils.implicits.Instances._

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage = SnapshotChunkMessage()
      .withChunks(chunk.nodesList)
      .withManifestId(ByteString.copyFrom(chunk.manifestId))
      .withId(ByteString.copyFrom(chunk.id))

    def fromProto(chunk: SnapshotChunkMessage): Try[SnapshotChunk] = Try(
      SnapshotChunk(
        chunk.chunks.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).toList,
        chunk.manifestId.toByteArray
      )
    )
  }

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class RequestActualManifest(remote: ConnectedPeer) extends AnyVal

  final case class ResponseManifest(manifest: Option[SnapshotManifest]) extends AnyVal

  final case class RequestSnapshot(remote: ConnectedPeer) extends AnyVal

  final case class ResponseSnapshotChunk(bytes: Array[Byte]) extends AnyVal

}