package encry.view.fastSync

import java.net.InetSocketAddress
import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.google.common.primitives.{Ints, Longs}
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.fastSync.SnapshotHolder._
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Serializer
import encry.view.state.avlTree.{Node, NodeSerilalizer}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.util.Try

class SnapshotHolder(settings: EncryAppSettings, networkController: ActorRef) extends Actor with StrictLogging {

  import context.dispatcher

  var snapshotProcessor: SnapshotProcessor = SnapshotProcessor.initialize(settings)

  override def preStart(): Unit = {
    logger.info(s"SnapshotHolder has started.")
    networkController ! RegisterMessagesHandler(Seq(
      RequestManifest.NetworkMessageTypeID -> "RequestManifest",
      ResponseManifestMessage.NetworkMessageTypeID -> "ResponseManifestMessage",
      RequestChunkMessage.NetworkMessageTypeID -> "RequestChunkMessage",
      ResponseChunkMessage.NetworkMessageTypeID -> "ResponseChunkMessage",
      ManifestHasChanged.NetworkMessageTypeID -> "ManifestHasChanged"
    ), self)
  }

  var communicationWithPeer: Option[(InetSocketAddress, Cancellable)] = None

  override def receive: Receive = {
    case UpdateSnapshot(block, state) =>
      logger.info(s"Snapshot holder got update snapshot message. Potential snapshot processing has started.")
      val newProcessor: SnapshotProcessor = snapshotProcessor.processNewSnapshot(state, block)
      snapshotProcessor = newProcessor

    case SemanticallySuccessfulModifier(block: Block) =>
      logger.info(s"Snapshot holder got semantically successful modifier message. Has started processing it.")
      val newProcessor: SnapshotProcessor = snapshotProcessor.processNewBlock(block)
      snapshotProcessor = newProcessor

    case SemanticallySuccessfulModifier(_) => //do nothing
    case nonsense => logger.info(s"Snapshot holder got strange message $nonsense.")
  }

  def fastSyncMod(toRequest: List[Array[Byte]],
                  manifestId: Array[Byte],
                  inAwait: List[Array[Byte]],
                  responseTimeout: Option[Cancellable]): Receive = {
    case AskNextChunks if inAwait.isEmpty =>

    case AskNextChunks =>
      context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! AskNextChunks)
    case DataFromPeer(message, remote) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case ResponseManifestMessage(byteString) =>
          SnapshotManifestSerializer.fromProto(byteString).fold(e => {
            logger.info(s"Tried to parse manifest from ${remote.socketAddress}, but gor exception ${e.getMessage}.")
            //todo ban node
          }, manifest => {
            logger.info(s"Got manifest from ${remote.socketAddress}. Starting fast sync downloading.")
            context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! AskNextChunks)
            context.become(fastSyncMod(manifest.chunksKeys, manifest.ManifestId, List.empty, None))
          })
        case ResponseChunkMessage(chunk) =>
        case ManifestHasChanged(requestedManifestId, newManifestId) =>
        case _ =>
      }

    case nonsense => logger.info(s"Snapshot holder got strange message $nonsense while fast sync mod.")
  }

  def workMod(connections: List[(InetSocketAddress, Cancellable)]): Receive = {
    case DataFromPeer(message, remote) if communicationWithPeer.exists(_._1 == remote.socketAddress) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case BasicMessagesRepo.RequestManifest =>
          logger.info(s"Remote ${remote.socketAddress} requested actual manifest.")
          snapshotProcessor.actualManifest.foreach { m =>
            logger.info(s"Sent to remote ${remote.socketAddress} actual manifest.")
            remote.handlerRef ! ResponseManifestMessage(SnapshotManifestSerializer.toProto(m))
          }
        case RequestChunkMessage(chunkId, manifestId) =>
          logger.info(s"Got request chunk message from ${remote.socketAddress} with manifest if ${Algos.encode(manifestId)}." +
            s" Actual manifest id is ${snapshotProcessor.actualManifest.map(e => Algos.encode(e.ManifestId))}.")

          if (
            snapshotProcessor.actualManifest.exists(_.ManifestId.sameElements(manifestId)) &&
              snapshotProcessor.actualManifest.exists(_.chunksKeys.exists(_.sameElements(chunkId)))
          ) {
            logger.info(s"Request for chunk from ${remote.socketAddress} is valid.")
            snapshotProcessor.getChunkById(chunkId).foreach { ch =>
              logger.info(s"Sent response with chunk ${Algos.encode(chunkId)} and manifest ${Algos.encode(manifestId)}.")
              communicationWithPeer = Some(
                remote.socketAddress ->
                  context.system.scheduler.scheduleOnce(settings.snapshotSettings.requestTimeout)(self ! TimeoutHasExpired)
              )
              remote.handlerRef ! ResponseChunkMessage(ch)
            }
          } else {
            //todo if 2nd condition false - ban node
            logger.info(s"Got request for chunk from old manifest.")
            snapshotProcessor.actualManifest.foreach { m =>
              logger.info(s"Sent to ${remote.socketAddress} new manifest.")
              ManifestHasChanged(manifestId, SnapshotManifestSerializer.toProto(m))
            }
          }
      }
    case TimeoutHasExpired =>
      communicationWithPeer = None
    //todo ban peer

    case DataFromPeer(message, remote) => //do nothing
  }

}

object SnapshotHolder {

  final case object TimeoutHasExpired
  final case object AskNextChunks

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

  def props(settings: EncryAppSettings, networkController: ActorRef): Props = Props(
    new SnapshotHolder(settings, networkController)
  )

}