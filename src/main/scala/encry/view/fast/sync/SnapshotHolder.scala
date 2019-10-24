package encry.view.fast.sync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{ Actor, ActorRef, Cancellable, Props }
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.Broadcast
import encry.network.NetworkController.ReceivableMessages.{ DataFromPeer, RegisterMessagesHandler }
import encry.network.PeersKeeper.{ BanPeer, SendToNetwork }
import encry.settings.EncryAppSettings
import SnapshotHolder._
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import cats.syntax.option._
import encry.network.BlackList.BanReason.{
  ExpiredNumberOfReRequestAttempts,
  ExpiredNumberOfRequests,
  InvalidChunkMessage,
  InvalidManifestHasChangedMessage,
  InvalidManifestMessage
}
import encry.view.history.History
import scala.util.Try

class SnapshotHolder(settings: EncryAppSettings,
                     networkController: ActorRef,
                     nodeViewHolder: ActorRef,
                     nodeViewSynchronizer: ActorRef)
    extends Actor
    with StrictLogging {

  import context.dispatcher

  //todo 1. Add connection agreement (case while peer reconnects with other handler.ref)

  var snapshotDownloadController: SnapshotDownloadController = SnapshotDownloadController.empty(settings)
  var connectionsHandler: IncomingConnectionsHandler         = IncomingConnectionsHandler.empty(settings)

  override def preStart(): Unit = {
    if (settings.snapshotSettings.newSnapshotCreationHeight <= settings.levelDB.maxVersions ||
        (!settings.snapshotSettings.enableFastSynchronization && !settings.snapshotSettings.enableSnapshotCreation)) {
      logger.info(s"Stop self(~_~)SnapshotHolder(~_~)")
      context.stop(self)
    }
    logger.info(s"SnapshotHolder started.")
    networkController ! RegisterMessagesHandler(
      Seq(
        RequestManifestMessage.NetworkMessageTypeID  -> "RequestManifest",
        ResponseManifestMessage.NetworkMessageTypeID -> "ResponseManifestMessage",
        RequestChunkMessage.NetworkMessageTypeID     -> "RequestChunkMessage",
        ResponseChunkMessage.NetworkMessageTypeID    -> "ResponseChunkMessage",
        ManifestHasChanged.NetworkMessageTypeID      -> "ManifestHasChanged"
      ),
      self
    )
  }

  override def receive: Receive = awaitingProcessor

  def awaitingProcessor: Receive = {
    case SnapshotProcessorAndHistory(processor, history) =>
      if (settings.snapshotSettings.enableFastSynchronization)
        context.become(
          fastSyncMod(history, needToProcessHeaderChainSyncedMessage = true, processor, none, reRequestsNumber = 0)
            .orElse(commonMessages)
        )
      else {
        context.system.scheduler
          .scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
        workMod(processor).orElse(commonMessages)
      }
  }

  def fastSyncMod(history: History,
                  needToProcessHeaderChainSyncedMessage: Boolean,
                  processor: SnapshotProcessor,
                  responseTimeout: Option[Cancellable],
                  reRequestsNumber: Int): Receive = {
    case RequestNextChunks =>
      responseTimeout.foreach(_.cancel())
      logger.info(s"Snapshot holder got RequestNextChunks message.")
      snapshotDownloadController.processRequestChunksMessage match {
        case Left(_) =>
          logger.info(s"Fast sync done in snapshot holder.")
          nodeViewHolder ! FastSyncDoneAt(
            snapshotDownloadController.requiredManifestHeight,
            history.getBestHeaderAtHeight { snapshotDownloadController.requiredManifestHeight }.map { header =>
              header.id
            }.getOrElse(Array.emptyByteArray)
          )
        case Right((controller, forRequest)) =>
          snapshotDownloadController = controller
          forRequest.foreach { msg =>
            controller.cp.foreach { peer =>
              peer.handlerRef ! msg
            }
          }
          val timer: Option[Cancellable] =
            context.system.scheduler.scheduleOnce(settings.snapshotSettings.responseTimeout)(self ! CheckDelivery).some
          context.become(
            fastSyncMod(history, needToProcessHeaderChainSyncedMessage, processor, timer, reRequestsNumber = 0)
          )
      }
    case DataFromPeer(message, remote) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case ResponseManifestMessage(manifest) =>
          snapshotDownloadController.processManifest(manifest, remote, history) match {
            case Left(error) =>
              logger.info(s"New manifest is incorrect. ${error.getMessage}.")
              nodeViewSynchronizer ! BanPeer(remote, InvalidManifestMessage(error.getMessage))
            case Right((newController, Some(rootNode))) =>
              snapshotDownloadController = newController
              nodeViewHolder ! ManifestInfoToNodeViewHolder(
                newController.requiredManifestId,
                newController.requiredManifestHeight,
                rootNode
              )
              self ! RequestNextChunks
            case Right(_) =>
          }
        case ResponseChunkMessage(chunk) =>
          snapshotDownloadController.processRequestedChunk(chunk, remote) match {
            case Left(error) =>
              logger.info(s"Received chunk is invalid cause ${error.getMessage}.")
              nodeViewSynchronizer ! BanPeer(remote, InvalidChunkMessage(error.getMessage))
            case Right((newProcessor, nodes)) if nodes.nonEmpty =>
              snapshotDownloadController = newProcessor
              nodeViewHolder ! NewChunkToApply(nodes)
              if (snapshotDownloadController.requestedChunks.isEmpty) {
                logger.info(s"Requested collection is empty. Request next chunks batch")
                self ! snapshotDownloadController
              }
            case Right(_) =>
              if (snapshotDownloadController.requestedChunks.isEmpty) {
                logger.info(s"Requested collection is empty. Request next chunks batch")
                self ! snapshotDownloadController
              }
          }
        case ManifestHasChanged(previousId, newManifest: SnapshotManifestProtoMessage) =>
          snapshotDownloadController.processManifestHasChangedMessage(previousId, newManifest, history, remote) match {
            case Left(error) =>
              logger.info(s"Manifest has changed message is incorrect ${error.getMessage}.")
              nodeViewSynchronizer ! BanPeer(remote, InvalidManifestHasChangedMessage(error.getMessage))
            case Right((newController, Some(rootNode))) =>
              snapshotDownloadController = newController
              nodeViewHolder ! ManifestInfoToNodeViewHolder(
                newController.requiredManifestId,
                newController.requiredManifestHeight,
                rootNode
              )
              self ! RequestNextChunks
            case Right(_) =>
          }
        case _ =>
      }
    case RequiredManifestHeightAndId(height, manifestId) =>
      logger.info(s"Snapshot holder while header sync got message RequiredManifestHeight with height $height.")
      snapshotDownloadController =
        snapshotDownloadController.copy(requiredManifestHeight = height, requiredManifestId = manifestId)
      logger.info(s"New required manifest id is ${Algos.encode(manifestId)}. Height is $height.")
    case HeaderChainIsSynced if needToProcessHeaderChainSyncedMessage =>
      logger.info(
        s"Snapshot holder got HeaderChainIsSynced. Broadcasts request for new manifest with id " +
          s"${Algos.encode(snapshotDownloadController.requiredManifestId)}"
      )
      nodeViewSynchronizer ! SendToNetwork(RequestManifestMessage(snapshotDownloadController.requiredManifestId),
                                           Broadcast)
      context.become(
        fastSyncMod(history, needToProcessHeaderChainSyncedMessage = false, processor, none, reRequestsNumber)
          .orElse(commonMessages)
      )
    case FastSyncDone if settings.snapshotSettings.enableSnapshotCreation =>
      logger.info(s"Snapshot holder context.become to snapshot processing")
      context.system.scheduler.scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
      context.become(workMod(processor).orElse(commonMessages))
    case CheckDelivery if reRequestsNumber < settings.snapshotSettings.reRequestAttempts =>
      snapshotDownloadController.requestedChunks.map { id =>
        RequestChunkMessage(id.data)
      }.foreach { msg =>
        snapshotDownloadController.cp.foreach(peer => peer.handlerRef ! msg)
      }
      val timer: Option[Cancellable] =
        context.system.scheduler.scheduleOnce(settings.snapshotSettings.responseTimeout)(self ! CheckDelivery).some
      context.become(
        fastSyncMod(history, needToProcessHeaderChainSyncedMessage, processor, timer, reRequestsNumber + 1)
      )
    case CheckDelivery =>
      snapshotDownloadController.cp.foreach { peer =>
        logger.info(s"Ban peer ${peer.socketAddress} for ExpiredNumberOfReRequestAttempts.")
        nodeViewSynchronizer ! BanPeer(peer, ExpiredNumberOfReRequestAttempts)
      }
      val newProcessor: SnapshotDownloadController = SnapshotDownloadController
        .empty(settings)
        .copy(requiredManifestId = snapshotDownloadController.requiredManifestId,
              requiredManifestHeight = snapshotDownloadController.requiredManifestHeight)
      snapshotDownloadController = newProcessor
      self ! HeaderChainIsSynced
      context.become(
        fastSyncMod(history, needToProcessHeaderChainSyncedMessage = true, processor, none, reRequestsNumber = 0)
          .orElse(commonMessages)
      )
    case FastSyncDone =>
      logger.info(s"Cannot processing new snapshots. Stopping snapshot holder...")
      context.stop(self)
  }

  def workMod(snapshotProcessor: SnapshotProcessor): Receive = {
    case DataFromPeer(message, remote) =>
      message match {
        case RequestManifestMessage(requiredManifestId)
            if connectionsHandler.canBeProcessed(snapshotProcessor, remote.socketAddress, requiredManifestId) =>
          logger.info(s"Got message RequestManifestMessage from ${remote.socketAddress}.")
          logger.info(s"Actual manifest with id ${Algos.encode(requiredManifestId)} is same as requested.")
          snapshotProcessor.actualManifest.foreach { m =>
            logger.info(s"Sent to remote actual manifest with id ${Algos.encode(requiredManifestId)}")
            remote.handlerRef ! ResponseManifestMessage(SnapshotManifestSerializer.toProto(m))
            connectionsHandler = connectionsHandler.addNewConnect(remote.socketAddress, m.chunksKeys.size)
          }

        case RequestManifestMessage(_) =>
        case RequestChunkMessage(chunkId) if connectionsHandler.canProcessResponse(remote.socketAddress) =>
          logger.info(s"Got RequestChunkMessage. Current handledRequests ${connectionsHandler.handledRequests}.")
          val chunkFromDB: Option[SnapshotChunkMessage] = snapshotProcessor.getChunkById(chunkId)
          chunkFromDB.foreach { chunk =>
            logger.info(s"Sent tp $remote chunk $chunk.")
            val networkMessage: NetworkMessage = ResponseChunkMessage(chunk)
            remote.handlerRef ! networkMessage
          }
          connectionsHandler = connectionsHandler.processRequest(remote.socketAddress)
        case RequestChunkMessage(_)
            if connectionsHandler.liveConnections.getOrElse(remote.socketAddress, 0 -> 0L)._1 == 0 =>
          logger.info(s"Ban peer $remote.")
          nodeViewSynchronizer ! BanPeer(remote, ExpiredNumberOfRequests)
          connectionsHandler = connectionsHandler.removeConnection(remote.socketAddress)
        case RequestChunkMessage(_) =>
        case _                      =>
      }
    case DropProcessedCount =>
      connectionsHandler = connectionsHandler.iterationProcessing
      context.system.scheduler.scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
  }

  def commonMessages: Receive = {
    case HeaderChainIsSynced               =>
    case RequiredManifestHeightAndId(_, _) =>
    case nonsense                          => logger.info(s"Snapshot holder got strange message $nonsense.")
  }
}

object SnapshotHolder {

  case object DropProcessedCount

  final case class RequiredManifestHeightAndId(height: Int, manifestId: Array[Byte])

  final case class SnapshotProcessorMessage(sp: SnapshotProcessor) extends AnyVal

  final case class SnapshotProcessorAndHistory(sp: SnapshotProcessor, history: History)

  final case class ManifestInfoToNodeViewHolder(manifestId: Array[Byte], blockHeight: Int, rootNode: NodeProtoMsg)

  final case class NewChunkToApply(list: List[NodeProtoMsg]) extends AnyVal

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class FastSyncDoneAt(height: Int, headerId: Array[Byte])

  case object FastSyncDone

  case object CheckDelivery

  case object RequestNextChunks

  case object HeaderChainIsSynced

  final case class SnapshotManifest(manifestId: Array[Byte],
                                    rootNodeBytes: NodeProtoMsg,
                                    chunksKeys: List[Array[Byte]]) {
    override def toString: String = s"Manifest id: ${Algos.encode(manifestId)}. Chunks size: ${chunksKeys.size}."
  }

  final case class SnapshotChunk(nodesList: List[NodeProtoMsg], id: Array[Byte]) {
    override def toString: String = s"Snapshot chunk id: ${Algos.encode(id)}. Nodes size: ${nodesList.size}."
  }

  object SnapshotManifestSerializer {

    def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage =
      SnapshotManifestProtoMessage()
        .withManifestId(ByteString.copyFrom(manifest.manifestId))
        .withRootNodeBytes(manifest.rootNodeBytes)
        .withChunksIds(manifest.chunksKeys.map(ByteString.copyFrom))

    def fromProto(manifest: SnapshotManifestProtoMessage): Try[SnapshotManifest] = Try(
      SnapshotManifest(
        manifest.manifestId.toByteArray,
        manifest.rootNodeBytes.get,
        manifest.chunksIds.map(_.toByteArray).toList
      )
    )
  }

  object SnapshotChunkSerializer extends StrictLogging {

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage =
      SnapshotChunkMessage()
        .withChunks(chunk.nodesList)
        .withId(ByteString.copyFrom(chunk.id))

    def fromProto(chunk: SnapshotChunkMessage): Try[SnapshotChunk] = Try(
      SnapshotChunk(chunk.chunks.toList, chunk.id.toByteArray)
    )
  }

  def props(settings: EncryAppSettings,
            networkController: ActorRef,
            nodeViewHolderRef: ActorRef,
            nodeViewSynchronizer: ActorRef): Props = Props(
    new SnapshotHolder(settings, networkController, nodeViewHolderRef, nodeViewSynchronizer)
  )

}
