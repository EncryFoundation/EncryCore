package encry.nvg.fast.sync

import akka.actor.{Actor, Cancellable}
import com.typesafe.scalalogging.StrictLogging
import encry.network.Messages.MessageToNetwork.BroadcastManifestRequest
import encry.nvg.fast.sync.SnapshotProcessor.{BroadcastManifestRequestMessage, RequiredManifestHeightAndId}
import encry.nvg.NodeViewHolder.SemanticallySuccessfulModifier
import encry.settings.EncryAppSettings
import encry.view.fast.sync.{SnapshotDownloadController, SnapshotHolder}
import encry.view.history.HistoryReader
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.Algos

class SnapshotDownloader(settings: EncryAppSettings) extends Actor with StrictLogging {

  import context.dispatcher

  var snapshotHolder: SnapshotHolder =
    SnapshotHolder.initialize(
      settings,
      if (settings.snapshotSettings.enableFastSynchronization) settings.storage.state
      else settings.storage.snapshotHolder
    )
  var snapshotDownloadController: SnapshotDownloadController = SnapshotDownloadController.empty(settings)
  var historyReader: HistoryReader                           = HistoryReader.empty
  var requiredManifestHeight: Int                            = 0

  override def receive: Receive = ???
  def receive1: Receive = {
    case SemanticallySuccessfulModifier(modifier) if modifier.modifierTypeId == Payload.modifierTypeId =>
      val bestBlockHeight: Int = historyReader.getBestBlockHeight
      if (historyReader.isFastSyncInProcess && bestBlockHeight >= requiredManifestHeight) {
        logger.info(
          s"Snapshot downloader got new block. Current best block height is: $bestBlockHeight. " +
            s"Height of last available payload for request is: $requiredManifestHeight."
        )
        historyReader
          .getBestHeaderAtHeight(requiredManifestHeight)
          .map { h: Header =>
            RequiredManifestHeightAndId(
              requiredManifestHeight,
              Algos.hash(h.stateRoot ++ h.id)
            )
          }
          .foreach { manifestToId: RequiredManifestHeightAndId =>
            logger.info(
              s"Manifest height is: ${manifestToId.height}. " +
                s"Manifest id is: ${Algos.encode(manifestToId.manifestId)}"
            )
            snapshotDownloadController = snapshotDownloadController.copy(
              requiredManifestHeight = manifestToId.height,
              requiredManifestId = manifestToId.manifestId
            )
          }
        restartFastSync()
        self ! BroadcastManifestRequestMessage
      }
    case SemanticallySuccessfulModifier(_) =>
    case BroadcastManifestRequestMessage =>
      logger.info(
        s"Snapshot downloader got BroadcastManifestRequestMessage message. " +
          s"Required manifest id is: ${Algos.encode(snapshotDownloadController.requiredManifestId)}."
      )
      context.parent ! BroadcastManifestRequest(snapshotDownloadController.requiredManifestId)

      val newScheduler: Cancellable =
        context.system.scheduler.scheduleOnce(settings.snapshotSettings.manifestReAskTimeout) {
          logger.info(s"Trigger scheduler for re-request manifest")
          self ! BroadcastManifestRequestMessage
        }
      logger.info(s"Start awaiting manifest network message.")
//      context.become(awaitingManifestFromNetwork(newScheduler).orElse(commonMessages))
  }

  def awaitingManifestFromNetwork(scheduler: Cancellable): Receive = {
    case _ =>
  }

//  def fastSyncMod(
//    history: History,
//    responseTimeout: Option[Cancellable]
//  ): Receive = {
//    case DataFromPeer(message, remote) =>
//      logger.debug(s"Snapshot holder got from ${remote} message ${message.NetworkMessageTypeID}.")
//      message match {
//        case ResponseManifestMessage(manifest) =>
//          logger.info(
//            s"Got new manifest message ${Algos.encode(manifest.manifestId.toByteArray)} while processing chunks."
//          )
//        case ResponseChunkMessage(chunk) if snapshotDownloadController.canChunkBeProcessed(remote) =>
//          (for {
//            controllerAndChunk  <- snapshotDownloadController.processRequestedChunk(chunk, remote)
//            (controller, chunk) = controllerAndChunk
//            validChunk          <- snapshotHolder.validateChunkId(chunk)
//            processor           = snapshotHolder.updateCache(validChunk)
//            newProcessor <- processor.processNextApplicableChunk(processor).leftFlatMap {
//                             case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
//                             case t                          => t.asLeft[SnapshotHolder]
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
//              snapshotHolder = processor
//              if (snapshotDownloadController.awaitedChunks.isEmpty) self ! RequestNextChunks
//          }
//
//        case ResponseChunkMessage(_) =>
//          logger.info(s"Received chunk from unexpected peer ${remote}")
//
//        case _ =>
//      }
//
//    case RequestNextChunks =>
//      responseTimeout.foreach(_.cancel())
//      (for {
//        controllerAndIds <- snapshotDownloadController.getNextBatchAndRemoveItFromController
//        _                = logger.info(s"Current notYetRequested batches is ${snapshotDownloadController.batchesSize}.")
//      } yield controllerAndIds) match {
//        case Left(err) =>
//          logger.info(s"Error has occurred: ${err.error}")
//          throw new Exception(s"Error has occurred: ${err.error}")
//        case Right(controllerAndIds) =>
//          snapshotDownloadController = controllerAndIds._1
//          controllerAndIds._2.foreach { msg =>
//            //            snapshotDownloadController.cp.foreach { peer: PeerConnectionHandler.ConnectedPeer =>
//            //              peer.handlerRef ! msg
//            //            }
//          }
//          context.become(fastSyncMod(history, timer).orElse(commonMessages))
//      }
//
//    case RequiredManifestHeightAndId(height, manifestId) =>
//      logger.info(
//        s"Snapshot holder while header sync got message RequiredManifestHeight with height $height." +
//          s"New required manifest id is ${Algos.encode(manifestId)}."
//      )
//      snapshotDownloadController = snapshotDownloadController.copy(
//        requiredManifestHeight = height,
//        requiredManifestId = manifestId
//      )
//      restartFastSync(history)
//      self ! BroadcastManifestRequestMessage
//      context.become(awaitManifestMod(none, history).orElse(commonMessages))
//
//    case CheckDelivery =>
//      snapshotDownloadController.awaitedChunks.map { id =>
//        RequestChunkMessage(id.data)
//      }.foreach { msg =>
//        //snapshotDownloadController.cp.foreach(peer => peer.handlerRef ! msg)
//      }
//      context.become(fastSyncMod(history, timer).orElse(commonMessages))
//
//    case FastSyncDone =>
//      if (settings.snapshotSettings.enableSnapshotCreation) {
//        logger.info(s"Snapshot holder context.become to snapshot processing")
//        snapshotHolder = SnapshotHolder.recreateAfterFastSyncIsDone(settings)
//        snapshotDownloadController.storage.close()
//        context.system.scheduler
//          .scheduleOnce(settings.snapshotSettings.updateRequestsPerTime)(self ! DropProcessedCount)
//        context.become(workMod(history).orElse(commonMessages))
//      } else {
//        logger.info(s"Stop processing snapshots")
//        context.stop(self)
//      }
//  }

  def restartFastSync(): Unit = {
    logger.info(s"Restart fast sync!")
    snapshotDownloadController = snapshotDownloadController.reInitFastSync
    snapshotHolder = snapshotHolder.reInitStorage
  }
}
