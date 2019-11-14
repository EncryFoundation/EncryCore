package encry.view.fast.sync

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
  InvalidResponseManifestMessage,
  InvalidStateAfterFastSync
}
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ ChangedHistory, SemanticallySuccessfulModifier }
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.fast.sync.FastSyncExceptions.{ ApplicableChunkIsAbsent, FastSyncException }
import encry.view.history.History
import encry.view.state.avlTree.{ Node, NodeSerilalizer }
import cats.syntax.either._
import scala.util.Try
import encry.view.state.avlTree.utils.implicits.Instances._

class SnapshotHolder(settings: EncryAppSettings,
                     networkController: ActorRef,
                     nodeViewHolder: ActorRef,
                     nodeViewSynchronizer: ActorRef)
    extends Actor
    with StrictLogging {

  import context.dispatcher

  //todo 1. Add connection agreement (case while peer reconnects with other handler.ref)

  var snapshotProcessor: SnapshotProcessor                   = SnapshotProcessor.initialize(settings)
  var snapshotDownloadController: SnapshotDownloadController = SnapshotDownloadController.empty(settings)
  var connectionsHandler: IncomingConnectionsHandler         = IncomingConnectionsHandler.empty(settings)

  override def preStart(): Unit = {
    if (settings.snapshotSettings.newSnapshotCreationHeight <= settings.levelDB.maxVersions ||
        (!settings.snapshotSettings.enableFastSynchronization && !settings.snapshotSettings.enableSnapshotCreation)) {
      logger.info(s"Stop self(~_~)SnapshotHolder(~_~)")
      context.stop(self)
    }
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
        context.become(
          fastSyncMod(history, processHeaderSyncedMsg = true, none, reRequestsNumber = 0).orElse(commonMessages)
        )
      } else {
        logger.info(s"Start in snapshot processing regime")
        context.system.scheduler
          .scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
        context.become(workMod(history).orElse(commonMessages))
      }
    case nonsense => logger.info(s"Snapshot holder got $nonsense while history awaiting")
  }

  def fastSyncMod(history: History,
                  processHeaderSyncedMsg: Boolean,
                  responseTimeout: Option[Cancellable],
                  reRequestsNumber: Int): Receive = {
    case DataFromPeer(message, remote) =>
      logger.info(s"Snapshot holder got from ${remote.socketAddress} message ${message.NetworkMessageTypeID}.")
      message match {
        case ResponseManifestMessage(manifest) =>
          val isValidManifest: Boolean =
            snapshotDownloadController.checkManifestValidity(manifest.manifestId.toByteArray, history)
          val canBeProcessed: Boolean = snapshotDownloadController.canNewManifestBeProcessed
          if (isValidManifest && canBeProcessed) {
            (for {
              controller <- snapshotDownloadController.processManifest(manifest, remote, history)
              processor <- snapshotProcessor.initializeApplicableChunksCache(
                            history,
                            snapshotDownloadController.requiredManifestHeight
                          )
            } yield (controller, processor)) match {
              case Left(error) =>
                nodeViewSynchronizer ! BanPeer(remote, InvalidResponseManifestMessage(error.error))
              case Right((controller, processor)) =>
                logger.info(s"Request manifest message successfully processed.")
                snapshotDownloadController = controller
                snapshotProcessor = processor
                self ! RequestNextChunks
            }
          } else if (!isValidManifest) {
            logger.info(s"Got manifest with invalid id ${Algos.encode(manifest.manifestId.toByteArray)}")
            nodeViewSynchronizer ! BanPeer(
              remote,
              InvalidResponseManifestMessage(s"Invalid manifest id ${Algos.encode(manifest.manifestId.toByteArray)}")
            )
          } else logger.info(s"Doesn't need to process new manifest.")
        case ResponseChunkMessage(chunk) if snapshotDownloadController.canChunkBeProcessed(remote) =>
          (for {
            controllerAndChunk  <- snapshotDownloadController.processRequestedChunk(chunk, remote)
            (controller, chunk) = controllerAndChunk
            validChunk          <- snapshotProcessor.validateChunkId(chunk)
            processor           = snapshotProcessor.updateCache(validChunk)
            newProcessor <- processor.processNextApplicableChunk(processor).leftFlatMap {
                             case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
                             case t                          => t.asLeft[SnapshotProcessor]
                           }
          } yield (newProcessor, controller)) match {
            case Left(error) =>
              nodeViewSynchronizer ! BanPeer(remote, InvalidChunkMessage(error.error))
              restartFastSync(history)
            case Right((processor, controller))
                if controller.requestedChunks.isEmpty && controller.notYetRequested.isEmpty && processor.chunksCache.nonEmpty =>
              nodeViewSynchronizer ! BanPeer(remote, InvalidChunkMessage("For request is empty, buffer is nonEmpty"))
              restartFastSync(history)
            case Right((processor, controller))
                if controller.requestedChunks.isEmpty && controller.notYetRequested.isEmpty =>
              processor.assembleUTXOState match {
                case Right(state) =>
                  logger.info(s"Tree is valid on Snapshot holder!")
                  (nodeViewHolder ! FastSyncFinished(state)).asRight[FastSyncException]
                case _ =>
                  nodeViewSynchronizer ! BanPeer(remote, InvalidStateAfterFastSync("State after fast sync is invalid"))
                  restartFastSync(history).asLeft[Unit]
              }
            case Right((processor, controller)) =>
              snapshotDownloadController = controller
              snapshotProcessor = processor
              if (snapshotDownloadController.requestedChunks.isEmpty) self ! RequestNextChunks
          }

        case ResponseChunkMessage(_) =>
          logger.info(s"Received chunk from unexpected peer ${remote.socketAddress}")
          nodeViewSynchronizer ! BanPeer(
            remote,
            InvalidChunkMessage(s"Received chunk from unexpected peer ${remote.socketAddress}")
          )

        case _ =>
      }

    case RequestNextChunks =>
      responseTimeout.foreach(_.cancel())
      logger.info(s"Current notYetRequested queue ${snapshotDownloadController.notYetRequested.size}.")
      val (newController, toDownload) = snapshotDownloadController.chunksIdsToDownload
      snapshotDownloadController = newController
      toDownload.foreach { msg =>
        snapshotDownloadController.cp.foreach { peer =>
          peer.handlerRef ! msg
        }
      }
      val timer: Option[Cancellable] =
        context.system.scheduler.scheduleOnce(settings.snapshotSettings.responseTimeout)(self ! CheckDelivery).some
      context.become(
        fastSyncMod(history, processHeaderSyncedMsg, timer, reRequestsNumber = 0)
      )

    case RequiredManifestHeightAndId(height, manifestId) =>
      logger.info(
        s"Snapshot holder while header sync got message RequiredManifestHeight with height $height." +
          s"New required manifest id is ${Algos.encode(manifestId)}."
      )
      snapshotDownloadController =
        snapshotDownloadController.copy(requiredManifestHeight = height, requiredManifestId = manifestId)
      restartFastSync(history)

    case HeaderChainIsSynced if processHeaderSyncedMsg =>
      logger.info(
        s"Snapshot holder got HeaderChainIsSynced. Broadcasts request for new manifest with id " +
          s"${Algos.encode(snapshotDownloadController.requiredManifestId)}"
      )
      nodeViewSynchronizer ! SendToNetwork(RequestManifestMessage(snapshotDownloadController.requiredManifestId),
                                           Broadcast)
      context.become(
        fastSyncMod(history, processHeaderSyncedMsg = false, none, reRequestsNumber).orElse(commonMessages)
      )

    case CheckDelivery if reRequestsNumber < settings.snapshotSettings.reRequestAttempts =>
      snapshotDownloadController.requestedChunks.map { id =>
        RequestChunkMessage(id.data)
      }.foreach { msg =>
        snapshotDownloadController.cp.foreach(peer => peer.handlerRef ! msg)
      }
      val timer: Option[Cancellable] =
        context.system.scheduler.scheduleOnce(settings.snapshotSettings.responseTimeout)(self ! CheckDelivery).some
      context.become(
        fastSyncMod(history, processHeaderSyncedMsg, timer, reRequestsNumber + 1)
      )

    case CheckDelivery =>
      snapshotDownloadController.cp.foreach { peer =>
        logger.info(s"Ban peer ${peer.socketAddress} for ExpiredNumberOfReRequestAttempts.")
        nodeViewSynchronizer ! BanPeer(peer, ExpiredNumberOfReRequestAttempts)
        restartFastSync(history)
      }

    case FastSyncDone =>
      if (settings.snapshotSettings.enableSnapshotCreation) {
        snapshotProcessor = SnapshotProcessor.recreate(settings)
        logger.info(s"Snapshot holder context.become to snapshot processing")
        context.system.scheduler
          .scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
        context.become(workMod(history).orElse(commonMessages))
      } else {
        logger.info(s"Stop processing snapshots")
        context.stop(self)
      }
  }

  def workMod(history: History): Receive = {
    case TreeChunks(chunks, id) =>
      //todo add collection with potentialManifestsIds to NVH
      val manifestIds: Seq[Array[Byte]] = snapshotProcessor.potentialManifestsIds
      if (!manifestIds.exists(_.sameElements(id))) {
        snapshotProcessor.createNewSnapshot(id, manifestIds, chunks)
      } else logger.info(s"Doesn't need to create snapshot")

    case SemanticallySuccessfulModifier(block: Block) if history.isFullChainSynced =>
      logger.info(s"Snapshot holder got semantically successful modifier message. Started processing it.")
      val condition: Int =
        (block.header.height - settings.levelDB.maxVersions) % settings.snapshotSettings.newSnapshotCreationHeight
      logger.info(s"condition = $condition")
      if (condition == 0) snapshotProcessor.processNewBlock(block, history) match {
        case Left(value)         =>
        case Right(newProcessor) => snapshotProcessor = newProcessor
      }

    case DataFromPeer(message, remote) =>
      message match {
        case RequestManifestMessage(requiredManifestId)
            if connectionsHandler.canBeProcessed(snapshotProcessor, remote, requiredManifestId) =>
          snapshotProcessor.actualManifest.foreach { m =>
            logger.info(s"Sent to remote actual manifest with id ${Algos.encode(requiredManifestId)}")
            remote.handlerRef ! ResponseManifestMessage(SnapshotManifestSerializer.toProto(m))
            connectionsHandler = connectionsHandler.addNewConnect(remote, m.chunksKeys.size)
          }
        case RequestManifestMessage(manifest) =>
          logger.info(s"Got request for manifest with ${Algos.encode(manifest)}")
        case RequestChunkMessage(chunkId) if connectionsHandler.canProcessResponse(remote) =>
          logger.info(s"Got RequestChunkMessage. Current handledRequests ${connectionsHandler.handledRequests}.")
          val chunkFromDB: Option[SnapshotChunkMessage] = snapshotProcessor.getChunkById(chunkId)
          chunkFromDB.foreach { chunk =>
            logger.info(s"Sent tp $remote chunk $chunk.")
            val networkMessage: NetworkMessage = ResponseChunkMessage(chunk)
            remote.handlerRef ! networkMessage
          }
          connectionsHandler = connectionsHandler.processRequest(remote)
        case RequestChunkMessage(_) if connectionsHandler.liveConnections.exists {
              case (peer, (lastRequests, _)) => peer.socketAddress == remote.socketAddress && lastRequests <= 0
            } =>
          logger.info(s"Ban peer $remote.")
          nodeViewSynchronizer ! BanPeer(remote, ExpiredNumberOfRequests)
          connectionsHandler = connectionsHandler.removeConnection(remote)
        case RequestChunkMessage(_) =>
        case _                      =>
      }
    case DropProcessedCount =>
      connectionsHandler = connectionsHandler.iterationProcessing
      context.system.scheduler.scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
  }

  def commonMessages: Receive = {
    case HeaderChainIsSynced               =>
    case SemanticallySuccessfulModifier(_) =>
    case nonsense                          => logger.info(s"Snapshot holder got strange message $nonsense.")
  }

  def restartFastSync(history: History): Unit = {
    logger.info(s"Restart fast sync!")
    val newController: SnapshotDownloadController = SnapshotDownloadController
      .empty(settings)
      .copy(
        requiredManifestHeight = snapshotDownloadController.requiredManifestHeight,
        requiredManifestId = snapshotDownloadController.requiredManifestId
      )
    snapshotDownloadController = newController
    snapshotProcessor = snapshotProcessor.reInitStorage
    self ! HeaderChainIsSynced
    context.become(
      fastSyncMod(history, processHeaderSyncedMsg = true, none, reRequestsNumber = 0).orElse(commonMessages)
    )
  }
}

object SnapshotHolder {

  final case class FastSyncFinished(state: UtxoState) extends AnyVal

  final case class TreeChunks(list: List[SnapshotChunk], id: Array[Byte])

  case object DropProcessedCount

  final case class RequiredManifestHeightAndId(height: Int, manifestId: Array[Byte])

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  case object FastSyncDone

  case object CheckDelivery

  case object RequestNextChunks

  case object HeaderChainIsSynced

  import encry.view.state.avlTree.utils.implicits.Instances._

  final case class SnapshotManifest(manifestId: Array[Byte], chunksKeys: List[Array[Byte]])

  final case class SnapshotChunk(node: Node[StorageKey, StorageValue], id: Array[Byte])

  object SnapshotManifestSerializer {

    def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage =
      SnapshotManifestProtoMessage()
        .withManifestId(ByteString.copyFrom(manifest.manifestId))
        .withChunksIds(manifest.chunksKeys.map(ByteString.copyFrom))

    def fromProto(manifest: SnapshotManifestProtoMessage): Try[SnapshotManifest] = Try(
      SnapshotManifest(
        manifest.manifestId.toByteArray,
        manifest.chunksIds.map(_.toByteArray).toList
      )
    )
  }

  object SnapshotChunkSerializer extends StrictLogging {

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage =
      SnapshotChunkMessage()
        .withChunk(NodeSerilalizer.toProto(chunk.node))
        .withId(ByteString.copyFrom(chunk.id))

    def fromProto[K, V](chunk: SnapshotChunkMessage): Try[SnapshotChunk] = Try(
      SnapshotChunk(NodeSerilalizer.fromProto(chunk.chunk.get), chunk.id.toByteArray)
    )
  }

  def props(settings: EncryAppSettings,
            networkController: ActorRef,
            nodeViewHolderRef: ActorRef,
            nodeViewSynchronizer: ActorRef): Props = Props(
    new SnapshotHolder(settings, networkController, nodeViewHolderRef, nodeViewSynchronizer)
  )

}
