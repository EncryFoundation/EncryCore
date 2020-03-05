package encry.view.fast.sync

import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{ Actor, ActorRef, Cancellable, Props }
import cats.syntax.option._
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.NetworkController.ReceivableMessages.{ DataFromPeer, RegisterMessagesHandler }
import encry.network.NodeViewSynchronizer.ReceivableMessages.ChangedHistory
import encry.network.PeersKeeper.SendToNetwork
import encry.network.{ Broadcast, PeerConnectionHandler }
import encry.nvg.NodeViewHolder.SemanticallySuccessfulModifier
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.fast.sync.SnapshotHolder.SnapshotManifest.{ ChunkId, ManifestId }
import encry.view.fast.sync.SnapshotHolder._
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.state.avlTree.{ Node, NodeSerilalizer }
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import supertagged.TaggedType

import scala.util.Try

class SnapshotHolder(settings: EncryAppSettings,
                     networkController: ActorRef,
                     nodeViewHolder: ActorRef,
                     nodeViewSynchronizer: ActorRef)
    extends Actor
    with StrictLogging {

  import context.dispatcher

  //todo 1. Add connection agreement (case while peer reconnects with other handler.ref)

  var snapshotProcessor: SnapshotProcessor =
    SnapshotProcessor.initialize(
      settings,
      if (settings.snapshotSettings.enableFastSynchronization) settings.storage.state
      else settings.storage.snapshotHolder
    )
  var snapshotDownloadController: SnapshotDownloadController = SnapshotDownloadController.empty(settings)
  var requestsProcessor: RequestsPerPeriodProcessor          = RequestsPerPeriodProcessor.empty(settings)

  override def preStart(): Unit =
    if (settings.constants.SnapshotCreationHeight <= settings.constants.MaxRollbackDepth ||
        (!settings.snapshotSettings.enableFastSynchronization && !settings.snapshotSettings.enableSnapshotCreation)) {
      logger.info(s"Stop self(~_~)SnapshotHolder(~_~)")
      context.stop(self)
    } else {
      context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
      logger.info(s"SnapshotHolder started.")
      networkController ! RegisterMessagesHandler(
        Seq(
          RequestManifestMessage.NetworkMessageTypeID  -> "RequestManifest",
          ResponseManifestMessage.NetworkMessageTypeID -> "ResponseManifestMessage",
          RequestChunkMessage.NetworkMessageTypeID     -> "RequestChunkMessage",
          ResponseChunkMessage.NetworkMessageTypeID    -> "ResponseChunkMessage"
        ),
        self
      )
    }

  override def receive: Receive = awaitingHistory

  def awaitingHistory: Receive = {
    case ChangedHistory(history) =>
      if (settings.snapshotSettings.enableFastSynchronization && !history.isBestBlockDefined &&
          !settings.node.offlineGeneration) {
        logger.info(s"Start in fast sync regime")
        context.become(fastSyncMod(history, none).orElse(commonMessages))
      } else {
        logger.info(s"Start in snapshot processing regime")
        context.system.scheduler
          .scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
        context.become(workMod(history).orElse(commonMessages))
      }
    case nonsense => logger.info(s"Snapshot holder got $nonsense while history awaiting")
  }

  def fastSyncMod(
    history: History,
    responseTimeout: Option[Cancellable]
  ): Receive = {
    case DataFromPeer(message, remote) =>
      logger.debug(s"Snapshot holder got from ${remote} message ${message.NetworkMessageTypeID}.")
      message match {
        case ResponseManifestMessage(manifest) =>
          logger.info(
            s"Got new manifest message ${Algos.encode(manifest.manifestId.toByteArray)} while processing chunks."
          )
//        case ResponseChunkMessage(chunk) if snapshotDownloadController.canChunkBeProcessed(remote) =>
//          (for {
//            controllerAndChunk  <- snapshotDownloadController.processRequestedChunk(chunk, remote)
//            (controller, chunk) = controllerAndChunk
//            validChunk          <- snapshotProcessor.validateChunkId(chunk)
//            processor           = snapshotProcessor.updateCache(validChunk)
//            newProcessor <- processor.processNextApplicableChunk(processor).leftFlatMap {
//                             case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
//                             case t                          => t.asLeft[SnapshotProcessor]
//                           }
//          } yield (newProcessor, controller)) match {
//            case Left(err: UnexpectedChunkMessage) =>
//              logger.info(s"Error has occurred ${err.error} with peer $remote")
//            case Left(error) =>
//              logger.info(s"Error has occurred: $error")
//              nodeViewSynchronizer ! BanPeer(remote, InvalidChunkMessage(error.error))
//              restartFastSync(history)
//            case Right((processor, controller))
//                if controller.awaitedChunks.isEmpty && controller.isBatchesSizeEmpty && processor.chunksCache.nonEmpty =>
//              nodeViewSynchronizer ! BanPeer(remote, InvalidChunkMessage("For request is empty, buffer is nonEmpty"))
//              restartFastSync(history)
//            case Right((processor, controller)) if controller.awaitedChunks.isEmpty && controller.isBatchesSizeEmpty =>
//              processor.assembleUTXOState() match {
//                case Right(state) =>
//                  logger.info(s"Tree is valid on Snapshot holder!")
//                  processor.wallet.foreach { wallet: EncryWallet =>
//                    (nodeViewHolder ! FastSyncFinished(state, wallet)).asRight[FastSyncException]
//                  }
//                case _ =>
//                  nodeViewSynchronizer ! BanPeer(remote, InvalidStateAfterFastSync("State after fast sync is invalid"))
//                  restartFastSync(history).asLeft[Unit]
//              }
//            case Right((processor, controller)) =>
//              snapshotDownloadController = controller
//              snapshotProcessor = processor
//              if (snapshotDownloadController.awaitedChunks.isEmpty) self ! RequestNextChunks
//          }

        case ResponseChunkMessage(_) =>
          logger.info(s"Received chunk from unexpected peer ${remote}")

        case _ =>
      }

    case RequestNextChunks =>
      responseTimeout.foreach(_.cancel())
      (for {
        controllerAndIds <- snapshotDownloadController.getNextBatchAndRemoveItFromController
        _                = logger.info(s"Current notYetRequested batches is ${snapshotDownloadController.batchesSize}.")
      } yield controllerAndIds) match {
        case Left(err) =>
          logger.info(s"Error has occurred: ${err.error}")
          throw new Exception(s"Error has occurred: ${err.error}")
        case Right(controllerAndIds) =>
          snapshotDownloadController = controllerAndIds._1
          controllerAndIds._2.foreach { msg =>
            snapshotDownloadController.cp.foreach { peer: PeerConnectionHandler.ConnectedPeer =>
              peer.handlerRef ! msg
            }
          }
          context.become(fastSyncMod(history, timer).orElse(commonMessages))
      }

    case RequiredManifestHeightAndId(height, manifestId) =>
      logger.info(
        s"Snapshot holder while header sync got message RequiredManifestHeight with height $height." +
          s"New required manifest id is ${Algos.encode(manifestId)}."
      )
      snapshotDownloadController = snapshotDownloadController.copy(
        requiredManifestHeight = height,
        requiredManifestId = manifestId
      )
      restartFastSync(history)
      self ! BroadcastManifestRequestMessage
      context.become(awaitManifestMod(none, history).orElse(commonMessages))

    case CheckDelivery =>
      snapshotDownloadController.awaitedChunks.map { id =>
        RequestChunkMessage(id.data)
      }.foreach { msg =>
        snapshotDownloadController.cp.foreach(peer => peer.handlerRef ! msg)
      }
      context.become(fastSyncMod(history, timer).orElse(commonMessages))

    case FastSyncDone =>
      if (settings.snapshotSettings.enableSnapshotCreation) {
        logger.info(s"Snapshot holder context.become to snapshot processing")
        snapshotProcessor = SnapshotProcessor.recreateAfterFastSyncIsDone(settings)
        snapshotDownloadController.storage.close()
        context.system.scheduler
          .scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
        context.become(workMod(history).orElse(commonMessages))
      } else {
        logger.info(s"Stop processing snapshots")
        context.stop(self)
      }
  }

  def awaitManifestMod(
    responseManifestTimeout: Option[Cancellable],
    history: History
  ): Receive = {
    case BroadcastManifestRequestMessage =>
      logger.info(
        s"Snapshot holder got HeaderChainIsSynced. Broadcasts request for new manifest with id " +
          s"${Algos.encode(snapshotDownloadController.requiredManifestId)}"
      )
      nodeViewSynchronizer ! SendToNetwork(RequestManifestMessage(snapshotDownloadController.requiredManifestId),
                                           Broadcast)
      val newScheduler = context.system.scheduler.scheduleOnce(settings.snapshotSettings.manifestReAskTimeout) {
        logger.info(s"Trigger scheduler for re-request manifest")
        self ! BroadcastManifestRequestMessage
      }
      logger.info(s"Start awaiting manifest network message.")
      context.become(awaitManifestMod(newScheduler.some, history).orElse(commonMessages))

    case DataFromPeer(message, remote) =>
//      message match {
//        case ResponseManifestMessage(manifest) =>
//          val isValidManifest: Boolean =
//            snapshotDownloadController.checkManifestValidity(manifest.manifestId.toByteArray, history)
//          val canBeProcessed: Boolean = snapshotDownloadController.canNewManifestBeProcessed
//          if (isValidManifest && canBeProcessed) {
//            (for {
//              controller <- snapshotDownloadController.processManifest(manifest, remote, history)
//              processor <- snapshotProcessor.initializeApplicableChunksCache(
//                            history,
//                            snapshotDownloadController.requiredManifestHeight
//                          )
//            } yield (controller, processor)) match {
//              case Left(error) =>
//                nodeViewSynchronizer ! BanPeer(remote, InvalidResponseManifestMessage(error.error))
//              case Right((controller, processor)) =>
//                logger.debug(s"Request manifest message successfully processed.")
//                responseManifestTimeout.foreach(_.cancel())
//                snapshotDownloadController = controller
//                snapshotProcessor = processor
//                self ! RequestNextChunks
//                logger.debug("Manifest processed successfully.")
//                context.become(fastSyncMod(history, none))
//            }
//          } else if (!isValidManifest) {
//            logger.info(s"Got manifest with invalid id ${Algos.encode(manifest.manifestId.toByteArray)}")
//            nodeViewSynchronizer ! BanPeer(
//              remote,
//              InvalidResponseManifestMessage(s"Invalid manifest id ${Algos.encode(manifest.manifestId.toByteArray)}")
//            )
//          } else logger.info(s"Doesn't need to process new manifest.")
//        case _ =>
//      }

    case msg @ RequiredManifestHeightAndId(_, _) =>
      self ! msg
      responseManifestTimeout.foreach(_.cancel())
      logger.info(s"Got RequiredManifestHeightAndId while awaitManifestMod")
      context.become(fastSyncMod(history, none))
  }

  def workMod(history: History): Receive = {
    case TreeChunks(chunks, id) =>
      val manifestIds: Seq[Array[Byte]] = snapshotProcessor.potentialManifestsIds
      if (!manifestIds.exists(_.sameElements(id))) {
        snapshotProcessor.createNewSnapshot(ManifestId @@ id, manifestIds, chunks)
      } else logger.info(s"Doesn't need to create snapshot")

    case SemanticallySuccessfulModifier(block: Block) if history.isFullChainSynced =>
      logger.info(s"Snapshot holder got semantically successful modifier message. Started processing it.")
      val condition: Int =
        (block.header.height - settings.constants.MaxRollbackDepth) % settings.constants.SnapshotCreationHeight
      logger.info(s"condition = $condition")
      if (condition == 0) snapshotProcessor.processNewBlock(block, history) match {
        case Left(_) =>
        case Right(newProcessor) =>
          snapshotProcessor = newProcessor
          requestsProcessor = RequestsPerPeriodProcessor.empty(settings)
          nodeViewHolder ! RemoveRedundantManifestIds
      }

    case DataFromPeer(message, remote) =>
      message match {
        case RequestManifestMessage(requiredManifestId)
            if requestsProcessor.canBeProcessed(snapshotProcessor, requiredManifestId) =>
          snapshotProcessor.actualManifest.foreach { m =>
            logger.info(s"Sent to remote actual manifest with id ${Algos.encode(requiredManifestId)}")
          //remote.handlerRef ! ResponseManifestMessage(SnapshotManifestSerializer.toProto(m))
          }
        case RequestManifestMessage(manifest) =>
          logger.debug(s"Got request for manifest with ${Algos.encode(manifest)}")
        case RequestChunkMessage(chunkId)
            //if requestsProcessor.canProcessRequest(remote)
            =>
          logger.debug(s"Got RequestChunkMessage. Current handledRequests ${requestsProcessor.handledRequests}.")
          val chunkFromDB: Option[SnapshotChunkMessage] = snapshotProcessor.getChunkById(chunkId)
          chunkFromDB.foreach { chunk =>
            logger.debug(s"Sent to $remote chunk $chunk.")
            val networkMessage: NetworkMessage = ResponseChunkMessage(chunk)
          //remote.handlerRef ! networkMessage
          }
        //requestsProcessor = requestsProcessor.processRequest(remote)
        case RequestChunkMessage(_) =>
        case _                      =>
      }
    case DropProcessedCount =>
      requestsProcessor = requestsProcessor.iterationProcessing
      context.system.scheduler.scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
  }

  def commonMessages: Receive = {
    case HeaderChainIsSynced               =>
    case SemanticallySuccessfulModifier(_) =>
    case nonsense                          => logger.info(s"Snapshot holder got strange message $nonsense.")
  }

  def restartFastSync(history: History): Unit = {
    logger.info(s"Restart fast sync!")
    snapshotDownloadController = snapshotDownloadController.reInitFastSync
    snapshotProcessor = snapshotProcessor.reInitStorage
  }

  def timer: Option[Cancellable] =
    context.system.scheduler.scheduleOnce(settings.snapshotSettings.responseTimeout)(self ! CheckDelivery).some
}

object SnapshotHolder {

  case object RemoveRedundantManifestIds

  final case object BroadcastManifestRequestMessage

  final case class FastSyncFinished(state: UtxoState, wallet: EncryWallet)

  final case class TreeChunks(list: List[SnapshotChunk], id: Array[Byte])

  case object DropProcessedCount

  final case class RequiredManifestHeightAndId(height: Int, manifestId: Array[Byte])

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  case object FastSyncDone

  case object CheckDelivery

  case object RequestNextChunks

  case object HeaderChainIsSynced

  import encry.view.state.avlTree.utils.implicits.Instances._

  final case class SnapshotManifest(manifestId: ManifestId, chunksKeys: List[ChunkId])
  object SnapshotManifest {
    type ChunkId = ChunkId.Type
    object ChunkId extends TaggedType[Array[Byte]]
    type ManifestId = ManifestId.Type
    object ManifestId extends TaggedType[Array[Byte]]
  }

  final case class SnapshotChunk(node: Node[StorageKey, StorageValue], id: ChunkId)

  object SnapshotManifestSerializer {

    def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage =
      SnapshotManifestProtoMessage()
        .withManifestId(ByteString.copyFrom(manifest.manifestId))
        .withChunksIds(manifest.chunksKeys.map(ByteString.copyFrom))

    def fromProto(manifest: SnapshotManifestProtoMessage): Try[SnapshotManifest] = Try(
      SnapshotManifest(
        ManifestId @@ manifest.manifestId.toByteArray,
        manifest.chunksIds.map(raw => ChunkId @@ raw.toByteArray).toList
      )
    )
  }

  object SnapshotChunkSerializer extends StrictLogging {

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage =
      SnapshotChunkMessage()
        .withChunk(NodeSerilalizer.toProto(chunk.node))
        .withId(ByteString.copyFrom(chunk.id))

    def fromProto[K, V](chunk: SnapshotChunkMessage): Try[SnapshotChunk] = Try(
      SnapshotChunk(NodeSerilalizer.fromProto(chunk.chunk.get), ChunkId @@ chunk.id.toByteArray)
    )
  }

  def props(settings: EncryAppSettings,
            networkController: ActorRef,
            nodeViewHolderRef: ActorRef,
            nodeViewSynchronizer: ActorRef): Props = Props(
    new SnapshotHolder(settings, networkController, nodeViewHolderRef, nodeViewSynchronizer)
  )

}
