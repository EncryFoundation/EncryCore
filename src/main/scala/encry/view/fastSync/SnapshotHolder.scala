package encry.view.fastSync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{ Actor, ActorRef, Cancellable, Props }
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.Broadcast
import encry.network.NetworkController.ReceivableMessages.{ DataFromPeer, RegisterMessagesHandler }
import encry.network.NodeViewSynchronizer.ReceivableMessages.ChangedHistory
import encry.network.PeersKeeper.SendToNetwork
import encry.settings.EncryAppSettings
import encry.view.fastSync.SnapshotHolder._
import encry.view.fastSync.SnapshotDownloadController.{
  ProcessNextRequestChunksMessageResult,
  ProcessRequestedChunkResult,
  ProcessRequestedManifestResult
}
import encry.view.state.UtxoState
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import cats.syntax.option._
import encry.view.history.History

import scala.util.Try
import scala.concurrent.duration._

class SnapshotHolder(settings: EncryAppSettings,
                     networkController: ActorRef,
                     nodeViewHolder: ActorRef,
                     nodeViewSynchronizer: ActorRef)
    extends Actor
    with StrictLogging {

  import context.dispatcher

  var snapshotProcessor: Option[SnapshotProcessor]           = None
  var snapshotDownloadController: SnapshotDownloadController = SnapshotDownloadController.empty(settings)
  var givingChunksProcessor: GivingChunksProcessor           = GivingChunksProcessor.empty

  override def preStart(): Unit = {
    if (settings.snapshotSettings.creationHeight <= settings.levelDB.maxVersions) context.stop(self)
    logger.info(s"SnapshotHolder has started.")
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
    if (settings.snapshotSettings.startWith) awaitingHistory.orElse(commonMessages)
    else workMod(None).orElse(commonMessages)

  def awaitingHistory: Receive = {
    case ChangedHistory(history) =>
      context.become(fastSyncMod(history, isHeaderChainSynced = false).orElse(commonMessages))
  }

  def fastSyncMod(history: History, isHeaderChainSynced: Boolean): Receive = {
    case RequestNextChunks =>
      snapshotDownloadController.processNextRequestChunksMessage() match {
        case ProcessNextRequestChunksMessageResult(controller, false, list: List[NetworkMessage]) if list.nonEmpty =>
          list.foreach { msg =>
            controller.cp.foreach { peer =>
              peer.handlerRef ! msg
            }
          }
          snapshotDownloadController = controller
          context.system.scheduler.scheduleOnce(1.seconds)(self ! RequestNextChunks)
        case ProcessNextRequestChunksMessageResult(_, true, _) =>
          nodeViewHolder ! FastSyncDoneAt(
            snapshotDownloadController.currentManifest.map(_.bestBlockHeight).getOrElse(-1),
            snapshotDownloadController.currentManifest.map(_.bestBlockId).getOrElse(Array.emptyByteArray)
          )
          context.become(workMod(None).orElse(commonMessages))
        case _ =>
      }
    case DataFromPeer(message, remote) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case ResponseManifestMessage(manifest) =>
          snapshotDownloadController.processManifest(manifest, remote, history) match {
            case ProcessRequestedManifestResult(_, true, false) => //ban peer
            case ProcessRequestedManifestResult(controller, false, true) =>
              snapshotDownloadController = controller
              nodeViewHolder ! ManifestToNodeViewHolder(snapshotDownloadController.currentManifest)
              self ! RequestNextChunks
            case ProcessRequestedManifestResult(_, false, false) => //do nothing
          }
        case ResponseChunkMessage(chunk) =>
          snapshotDownloadController.processRequestedChunk(chunk, remote) match {
            case ProcessRequestedChunkResult(_, true, _) =>
            case ProcessRequestedChunkResult(controller, false, list: List[NodeProtoMsg])
                if list.nonEmpty => //todo bug with emptyChunk
              snapshotDownloadController = controller
              nodeViewHolder ! NewChunkToApply(list)
            case ProcessRequestedChunkResult(controller, false, _) => snapshotDownloadController = controller
          }
        case ManifestHasChanged(requestedManifestId, newManifest: SnapshotManifestProtoMessage) =>
          snapshotDownloadController.processManifest(newManifest,
                                                     remote,
                                                     history,
                                                     manifestHasChangedMessage = true,
                                                     requestedManifestId) match {
            case ProcessRequestedManifestResult(_, true, false) => //ban peer
            case ProcessRequestedManifestResult(controller, false, true) =>
              snapshotDownloadController = controller
              nodeViewHolder ! ManifestToNodeViewHolder(snapshotDownloadController.currentManifest)
              self ! RequestNextChunks
            case ProcessRequestedManifestResult(_, false, false) => //do nothing
          }
        case _ =>
      }
    case HeaderChainIsSynced if !isHeaderChainSynced =>
      logger.info(s"Broadcast request for new manifest")
      nodeViewSynchronizer ! SendToNetwork(RequestManifest, Broadcast)
      context.become(fastSyncMod(history, isHeaderChainSynced = true).orElse(commonMessages))
    case HeaderChainIsSynced =>
  }

  def workMod(timeout: Option[Cancellable]): Receive = {
    case DataFromPeer(message, remote) if givingChunksProcessor.canPeerBeProcessed(remote.socketAddress) =>
      message match {
        case RequestManifest =>
          val actualManifest: Option[SnapshotManifest] = snapshotProcessor.flatMap(_.actualManifest)
          logger.info(s"Got request from ${remote.socketAddress} for actual manifest")
          actualManifest.foreach { m =>
            logger.info(s"Sent to remote ${remote.socketAddress} actual manifest")
            remote.handlerRef ! ResponseManifestMessage(SnapshotManifestSerializer.toProto(m))
          }
          givingChunksProcessor = givingChunksProcessor.copy(
            Some(remote),
            actualManifest,
            actualManifest.map(_.chunksKeys).getOrElse(List.empty).map(ByteArrayWrapper(_)).toSet
          )
        case RequestChunkMessage(chunkId, manifestId) if givingChunksProcessor.processChunk(chunkId, manifestId) =>
          timeout.foreach(_.cancel())
          logger.info(
            s"Got request chunk message from ${remote.socketAddress} with manifest if ${Algos.encode(manifestId)}." +
              s" Actual manifest id is ${givingChunksProcessor.manifest.map(e => Algos.encode(e.ManifestId))}."
          )
          snapshotProcessor.foreach { processor =>
            val chunkFromDB: Option[SnapshotChunkMessage] = processor.getChunkById(chunkId)
            if (chunkFromDB.isEmpty) {
              val newActualManifest: Option[SnapshotManifest] = processor.actualManifest
              newActualManifest.foreach { manifest =>
                logger.info(s"Manifest has changed. Sent to remote ${remote.socketAddress} new actual manifest")
                remote.handlerRef ! ManifestHasChanged(manifestId, SnapshotManifestSerializer.toProto(manifest))
              }
              givingChunksProcessor = givingChunksProcessor.copy(
                remote.some,
                newActualManifest,
                newActualManifest.map(_.chunksKeys).getOrElse(List.empty).map(ByteArrayWrapper(_)).toSet
              )
            } else {
              chunkFromDB.foreach { chunk =>
                logger.info(s"Sent to remote ${remote.socketAddress} chunk ${Algos.encode(chunk.id.toByteArray)}")
                val networkMessage: NetworkMessage = ResponseChunkMessage(chunk)
                remote.handlerRef ! networkMessage
              }
              context.become(
                workMod(
                  context.system.scheduler
                    .scheduleOnce(settings.snapshotSettings.requestTimeout)(self ! TimeoutHasExpired)
                    .some
                )
              )
            }
          }
        case RequestChunkMessage(_, manifestId) =>
          //todo if chunkId doesn't contain in notYetRequested - ban peer
          logger.info(s"Got request for chunk from old manifest")
          timeout.foreach(_.cancel())
          val newActualManifest: Option[SnapshotManifest] = snapshotProcessor.flatMap(_.actualManifest)
          snapshotProcessor.foreach { processor =>
            processor.manifestBytesById(processor.ActualManifestKey).foreach { bytes =>
              logger.info(s"Sent to ${remote.socketAddress} new manifest.")
              ManifestHasChanged(manifestId, SnapshotManifestProtoMessage.parseFrom(bytes))
            }
          }
          givingChunksProcessor = givingChunksProcessor.copy(
            remote.some,
            newActualManifest,
            newActualManifest.map(_.chunksKeys).getOrElse(List.empty).map(ByteArrayWrapper(_)).toSet
          )
        case _ =>
      }
    case TimeoutHasExpired => givingChunksProcessor = GivingChunksProcessor.empty
  }

  def commonMessages: Receive = {
    case SnapshotProcessorMessage(processor) => snapshotProcessor = processor.some
    case nonsense                            => logger.info(s"Snapshot holder got strange message $nonsense.")
  }
}

object SnapshotHolder {

  final case class SnapshotProcessorMessage(sp: SnapshotProcessor)

  final case class ManifestToNodeViewHolder(m: Option[SnapshotManifest]) extends AnyVal

  final case class NewChunkToApply(list: List[NodeProtoMsg]) extends AnyVal

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class FastSyncDoneAt(height: Int, headerId: Array[Byte])

  case object FastSyncDone

  case object TimeoutHasExpired

  case object RequestNextChunks

  case object HeaderChainIsSynced

  final case class SnapshotManifest(bestBlockId: ModifierId,
                                    rootHash: Array[Byte],
                                    rootNodeBytes: NodeProtoMsg,
                                    stateChunksNumber: Long,
                                    bestBlockHeight: Int,
                                    chunksKeys: List[Array[Byte]]) {
    val ManifestId: Array[Byte] = Algos.hash(rootHash ++ bestBlockId)

    override def toString: String =
      s"bestBlockId ${Algos.encode(bestBlockId)}, rootHash ${Algos.encode(rootHash)}, " +
        s"stateChunksNumber $stateChunksNumber, bestBlockHeight $bestBlockHeight."
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

  object SnapshotChunkSerializer extends StrictLogging {

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage =
      SnapshotChunkMessage()
        .withChunks(chunk.nodesList)
        .withManifestId(ByteString.copyFrom(chunk.manifestId))
        .withId(ByteString.copyFrom(chunk.id))

    def fromProto(chunk: SnapshotChunkMessage): Try[SnapshotChunk] = Try(
      SnapshotChunk(chunk.chunks.toList, chunk.manifestId.toByteArray, chunk.id.toByteArray)
    )
  }

  def props(settings: EncryAppSettings,
            networkController: ActorRef,
            nodeViewHolderRef: ActorRef,
            nodeViewSynchronizer: ActorRef): Props = Props(
    new SnapshotHolder(settings, networkController, nodeViewHolderRef, nodeViewSynchronizer)
  )

}
