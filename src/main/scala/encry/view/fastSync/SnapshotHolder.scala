package encry.view.fastSync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{ Actor, ActorRef, Cancellable, Props }
import com.google.common.primitives.{ Ints, Longs }
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.Broadcast
import encry.network.NetworkController.ReceivableMessages.{ DataFromPeer, RegisterMessagesHandler }
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.SendToNetwork
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.fastSync.SnapshotHolder._
import encry.view.fastSync.SnapshotDownloadController.{
  ProcessManifestHasChangedMessage,
  ProcessRequestedChunkResult,
  ProcessRequestedManifestResult
}
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Serializer
import encry.view.state.avlTree.{ Node, NodeSerilalizer }
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.util.Try

class SnapshotHolder(settings: EncryAppSettings,
                     networkController: ActorRef,
                     nodeViewHolder: ActorRef,
                     nodeViewSyncronizer: ActorRef)
    extends Actor
    with StrictLogging {

  import context.dispatcher

  var snapshotProcessor: SnapshotProcessor                   = SnapshotProcessor.initialize(settings)
  var snapshotDownloadController: SnapshotDownloadController = SnapshotDownloadController.empty(settings)
  var givingChunksProcessor: GivingChunksProcessor           = GivingChunksProcessor.empty

  var processedTmp = false

  override def preStart(): Unit = {
    logger.info(s"SnapshotHolder has started.")
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
    networkController ! RegisterMessagesHandler(
      Seq(
        RequestManifest.NetworkMessageTypeID         -> "RequestManifest",
        ResponseManifestMessage.NetworkMessageTypeID -> "ResponseManifestMessage",
        RequestChunkMessage.NetworkMessageTypeID     -> "RequestChunkMessage",
        ResponseChunkMessage.NetworkMessageTypeID    -> "ResponseChunkMessage",
        ManifestHasChanged.NetworkMessageTypeID      -> "ManifestHasChanged"
      ),
      self
    )
  }

  override def receive: Receive =
    if (settings.snapshotSettings.startWith) fastSyncMod
    else workMod(None).orElse(commonMessages)

  def fastSyncMod: Receive = {
    case RequestNextChunks
        if snapshotDownloadController.toRequest.nonEmpty || snapshotDownloadController.inAwait.nonEmpty =>
      logger.info(s"Got RequestNextChunks message.")
      val (controller, toDownload) = snapshotDownloadController.chunksIdsToDownload
      logger.info(s"Chunk lasts: all size ${controller.toRequest.size}. inAwait: ${controller.inAwait.size}")
      if (toDownload.nonEmpty) toDownload.foreach { b =>
        logger.info(s"toDownload.nonEmpty -> true")
        controller.cp.foreach { l =>
          logger.info(s"controller.cp -> nonEmpty: true")
          l.handlerRef ! b
        }
      }
      snapshotDownloadController = controller
      context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! RequestNextChunks)

    case RequestNextChunks if snapshotDownloadController.currentManifest.nonEmpty =>
      nodeViewHolder ! FastSyncDoneAt(
        snapshotProcessor.actualManifest.map(_.bestBlockHeight).getOrElse(-1),
        snapshotProcessor.actualManifest.map(_.bestBlockId).getOrElse(Array.emptyByteArray)
      )

    case DataFromPeer(message, remote) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case ResponseManifestMessage(manifest) =>
          snapshotDownloadController.processRequestedManifest(manifest, remote) match {
            case ProcessRequestedManifestResult(_, true, false) =>
              logger.info(s"Ban illegal peer ${remote.socketAddress}!")
            //todo ban remote
            case ProcessRequestedManifestResult(controller, false, true) =>
              logger.info(s"Start processing chunks for manifest ${controller.currentManifest
                .map(e => Algos.encode(e.ManifestId))} from ${remote.socketAddress}.")
              snapshotDownloadController = controller
              self ! RequestNextChunks
            case ProcessRequestedManifestResult(_, false, false) =>
              logger.info(s"Got new manifest from ${remote.socketAddress} but has been already processing other one.")
          }
        case ResponseChunkMessage(chunk) =>
          logger.info(s"Got DataFromPeer -> ResponseChunkMessage -> with chunk ${Algos.encode(chunk.id.toByteArray)} and manifestId ->" +
            s" ${Algos.encode(chunk.manifestId.toByteArray)}")
          snapshotDownloadController.processRequestedChunk(chunk, remote) match {
            case ProcessRequestedChunkResult(_, true, _) =>
              logger.info(s"Got corrupted chunk from ${remote.socketAddress}.")
            //todo ban node

            case ProcessRequestedChunkResult(controller, false, list: List[NodeProtoMsg]) if list.nonEmpty =>
              logger.info(s"Got correct chunk from ${remote.socketAddress}.")
              snapshotDownloadController = controller
              nodeViewHolder ! NewChunkToApply(list)
            case ProcessRequestedChunkResult(_, false, _) =>
              logger.info(s"Got chunk from peer which not correspond to expected.")
          }
        case ManifestHasChanged(requestedManifestId, newManifestId: SnapshotManifestProtoMessage) =>
          snapshotDownloadController.processManifestHasChangedMessage(newManifestId, requestedManifestId, remote) match {
            case ProcessManifestHasChangedMessage(_, true) =>
            //todo ban peer
            case ProcessManifestHasChangedMessage(controller, false) =>
              snapshotDownloadController = controller
          }
        case _ =>
      }

    case HeaderChainIsSynced if !processedTmp => //todo probably should process only 1 time
      processedTmp = true
      logger.info(s"Broadcast request for new manifest")
      nodeViewSyncronizer ! SendToNetwork(RequestManifest, Broadcast)
    case HeaderChainIsSynced               =>
    case SemanticallySuccessfulModifier(_) =>
    case RequestNextChunks                 =>
    case nonsense                          => logger.info(s"Snapshot holder got strange message $nonsense while fast sync mod.")
  }

  def workMod(timeout: Option[Cancellable]): Receive = {
    case DataFromPeer(message, remote)
        if givingChunksProcessor.peer.isEmpty || givingChunksProcessor.peer.exists(
          _.socketAddress == remote.socketAddress
        ) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case BasicMessagesRepo.RequestManifest =>
          logger.info(s"Remote ${remote.socketAddress} requested actual manifest.")
          snapshotProcessor.actualManifest.foreach { m =>
            logger.info(s"Sent to remote ${remote.socketAddress} actual manifest.")
            remote.handlerRef ! ResponseManifestMessage(SnapshotManifestSerializer.toProto(m))
          }
          givingChunksProcessor = givingChunksProcessor
            .copy(Some(remote),
                  snapshotProcessor.actualManifest,
                  snapshotProcessor.actualManifest.map(_.chunksKeys).getOrElse(List.empty))
        case RequestChunkMessage(chunkId, manifestId) =>
          timeout.foreach(_.cancel())
          logger.info(
            s"Got request chunk message from ${remote.socketAddress} with manifest if ${Algos.encode(manifestId)}." +
              s" Actual manifest id is ${snapshotProcessor.actualManifest.map(e => Algos.encode(e.ManifestId))}."
          )
          if (givingChunksProcessor.peer.exists(_.socketAddress == remote.socketAddress) && snapshotProcessor.actualManifest
                .exists(_.ManifestId.sameElements(manifestId)) && snapshotProcessor.actualManifest
                .exists(_.chunksKeys.exists(_.sameElements(chunkId)))) {
            logger.info(s"Request for chunk from ${remote.socketAddress} is valid.")
            snapshotProcessor.getChunkById(chunkId).foreach { ch =>
              logger
                .info(s"Sent response with chunk ${Algos.encode(chunkId)} and manifest ${Algos.encode(manifestId)}.")
              givingChunksProcessor = givingChunksProcessor.updateLastsIds(ch.id.toByteArray, remote)
              val a = ResponseChunkMessage(ch)
              logger.info(s"ResponseChunkMessage for chunk ${Algos.encode(chunkId)} is ${Algos.encode(a.chunk.id.toByteArray)}" +
                s" with manifestId ${Algos.encode(a.chunk.manifestId.toByteArray)}")
              remote.handlerRef ! a
            }
            context.become(
              workMod(
                Some(
                  context.system.scheduler
                    .scheduleOnce(settings.snapshotSettings.requestTimeout)(self ! TimeoutHasExpired)
                )
              )
            )
          } else if (givingChunksProcessor.peer.exists(_.socketAddress == remote.socketAddress)) {
            //todo if 2nd condition false - ban node
            logger.info(s"Got request for chunk from old manifest.")
            snapshotProcessor.actualManifest.foreach { m =>
              logger.info(s"Sent to ${remote.socketAddress} new manifest.")
              ManifestHasChanged(manifestId, SnapshotManifestSerializer.toProto(m))
            }
            givingChunksProcessor = givingChunksProcessor.copy(
              Some(remote),
              snapshotProcessor.actualManifest,
              snapshotProcessor.actualManifest.map(_.chunksKeys).getOrElse(List.empty)
            )
            context.become(
              workMod(
                Some(
                  context.system.scheduler
                    .scheduleOnce(settings.snapshotSettings.requestTimeout)(self ! TimeoutHasExpired)
                )
              )
            )
          } else { /*???*/ }
        case _ =>
      }

    case TimeoutHasExpired => givingChunksProcessor = GivingChunksProcessor.empty
  }

  def commonMessages: Receive = {
    case UpdateSnapshot(block, state) if block.header.height != settings.constants.GenesisHeight =>
      logger.info(s"Snapshot holder got update snapshot message. Potential snapshot processing has started.")
      val newProcessor: SnapshotProcessor = snapshotProcessor.processNewSnapshot(state, block)
      snapshotProcessor = newProcessor

    case UpdateSnapshot(_, _) =>
      logger.info(s"Got update state for genesis block and tree.")

    case SemanticallySuccessfulModifier(block: Block) =>
      logger.info(s"Snapshot holder got semantically successful modifier message. Has started processing it.")
      val newProcessor: SnapshotProcessor = snapshotProcessor.processNewBlock(block)
      snapshotProcessor = newProcessor

    case SemanticallySuccessfulModifier(_) => //do nothing
    case nonsense                          => logger.info(s"Snapshot holder got strange message $nonsense.")
  }
}

object SnapshotHolder {

  final case object TimeoutHasExpired

  final case object RequestNextChunks

  final case object HeaderChainIsSynced

  final case class NewChunkToApply(list: List[NodeProtoMsg]) extends AnyVal

  final case class SnapshotManifest(bestBlockId: ModifierId,
                                    rootHash: Array[Byte],
                                    rootNodeBytes: NodeProtoMsg,
                                    stateChunksNumber: Long,
                                    bestBlockHeight: Int,
                                    chunksKeys: List[Array[Byte]]) {
    val ManifestId: Array[Byte] = Algos.hash(
      Longs.toByteArray(stateChunksNumber) ++ bestBlockId ++ rootHash ++
        rootNodeBytes.toByteArray ++ Ints.toByteArray(bestBlockHeight)
    )
  }

  object SnapshotManifestSerializer {

    def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage =
      SnapshotManifestProtoMessage()
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

  final case class SnapshotChunk(nodesList: List[NodeProtoMsg], manifestId: Array[Byte], id: Array[Byte])

  object SnapshotChunk extends StrictLogging  {
    def apply[K: Serializer, V: Serializer](list: List[Node[K, V]]): SnapshotChunk = {
      val chunkId: Array[Byte] = Algos.hash(list.flatMap(_.hash).toArray)
      new SnapshotChunk(
        list.map(NodeSerilalizer.toProto(_)),
        Array.emptyByteArray,
        chunkId
      )
    }

    def apply[K: Serializer, V: Serializer](list: List[Node[K, V]], manifestId: Array[Byte]): SnapshotChunk = {
      val chunkId: Array[Byte] = Algos.hash(list.flatMap(_.hash).toArray ++ manifestId)
      logger.info(s"Create snapshotChunk with id ${Algos.encode(chunkId)}")
      new SnapshotChunk(
        list.map(NodeSerilalizer.toProto(_)),
        manifestId,
        chunkId
      )
    }
  }

  object SnapshotChunkSerializer extends StrictLogging {

    import encry.view.state.avlTree.utils.implicits.Instances._

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage =
      SnapshotChunkMessage()
        .withChunks(chunk.nodesList)
        .withManifestId(ByteString.copyFrom(chunk.manifestId))
        .withId(ByteString.copyFrom(chunk.id))

    def fromProto(chunk: SnapshotChunkMessage): Try[SnapshotChunk] = Try {
      val a = SnapshotChunk(
        chunk.chunks.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).toList,
        chunk.manifestId.toByteArray
      )
      logger.info(s"From proto for chunk ${Algos.encode(chunk.id.toByteArray)} created SnapshotChunk with id ${Algos.encode(a.id)}")
      logger.info(s"From proto for chunk ${Algos.encode(chunk.id.toByteArray)} with manifest id ${Algos.encode(chunk.manifestId.toByteArray)}" +
        s" created SnapshotChunk with manifest id ${Algos.encode(a.manifestId)}")
      a
    }
  }

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class RequestActualManifest(remote: ConnectedPeer) extends AnyVal

  final case class ResponseManifest(manifest: Option[SnapshotManifest]) extends AnyVal

  final case class RequestSnapshot(remote: ConnectedPeer) extends AnyVal

  final case class ResponseSnapshotChunk(bytes: Array[Byte]) extends AnyVal

  final case class FastSyncDoneAt(height: Int, headerId: Array[Byte])

  final case object FastSyncDone

  def props(settings: EncryAppSettings,
            networkController: ActorRef,
            nodeViewHolderRef: ActorRef,
            nodeViewSyncronizer: ActorRef): Props = Props(
    new SnapshotHolder(settings, networkController, nodeViewHolderRef, nodeViewSyncronizer)
  )

}
