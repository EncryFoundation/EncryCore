package encry.view.actors

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.{miner, nodeViewSynchronizer}
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallyFailedModification, UpdatedHistory}
import encry.settings.EncryAppSettings
import encry.view.ModifiersCache
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.DownloadRequest
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator.{NeedToReportAsValid, NotificationAboutSuccessfullyAppliedModifier, RequestNextModifier}
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.collection.immutable.Queue
import scala.collection.{Seq, mutable}

class HistoryApplicator(history: History,
                        state: UtxoState,
                        setting: EncryAppSettings) extends Actor with StrictLogging {

  val stateApplicator: ActorRef = context.system.actorOf(
    StateApplicator.props(setting, history, self, state).withDispatcher("state-applicator-dispatcher"),
    name = "stateApplicator"
  )

  var modifiersQueue: Queue[(String, ProgressInfo)] = Queue.empty[(String, ProgressInfo)]
  var currentNumberOfAppliedModifiers: Int = 0

  override def receive: Receive = {
    case ModifierFromNetwork(mod) if !history.isModifierDefined(mod.id) && !ModifiersCache.contains(toKey(mod.id)) =>
      logger.debug(s"Got new modifier ${mod.encodedId} of type ${mod.modifierTypeId} from network." +
        s" Put it into modifiers cache.")
      ModifiersCache.put(toKey(mod.id), mod, history)
      getModifierForApplying()

    case ModifierFromNetwork(modifier) =>
      logger.info(s"Modifier ${modifier.encodedId} contains in history or in modifiers cache. Reject it.")

    case NeedToReportAsValid(modifier) =>
      logger.info(s"Modifier ${modifier.encodedId} should be marked as valid.")
      history.reportModifierIsValid(modifier)

    case ModifierToHistoryAppending(modifier) =>
      logger.info(s"Starting to apply modifier ${modifier.encodedId} to history.")
      history.append(modifier) match {
        case Left(ex) =>
          currentNumberOfAppliedModifiers -= 1
          logger.info(s"Modifier ${modifier.encodedId} unsuccessfully applied to history with exception ${ex.getMessage}." +
            s" Current currentNumberOfAppliedModifiers $currentNumberOfAppliedModifiers.")
          context.system.eventStream.publish(SyntacticallyFailedModification(modifier, List(HistoryApplyError(ex.getMessage))))
        //todo add behaviour in failed case
        case Right(progressInfo) if progressInfo.toApply.nonEmpty =>
          logger.info(s"Modifier ${modifier.encodedId} successfully applied to history.")
          modifiersQueue = modifiersQueue.enqueue(modifier.encodedId -> progressInfo)
          logger.info(s"New element put into queue. Current queue size is ${modifiersQueue.length}." +
            s"Current number of applied modifiers is $currentNumberOfAppliedModifiers.")
          stateApplicator ! NewModifierNotification
          getModifierForApplying()
        case Right(progressInfo) =>
          logger.info(s"Progress info is empty")
          requestDownloads(progressInfo, Some(modifier.id))
          //history.reportModifierIsValid(mod)
          context.system.eventStream.publish(SemanticallySuccessfulModifier(modifier))
          getModifierForApplying()
          currentNumberOfAppliedModifiers -= 1
      }

    case RequestNextModifier =>
      logger.info(s"Got request for the new next modifier")
      val nextModifier: Option[((String, ProgressInfo), Queue[(String, ProgressInfo)])] =
        modifiersQueue.dequeueOption
      nextModifier.foreach { case ((mod, pi), newQueue) =>
        logger.info(s"Found new valid for state modifier $mod. Send it to the state applicator.")
        sender() ! StartModifiersApplicationOnStateApplicator(pi, IndexedSeq.empty[PersistentModifier])
        modifiersQueue = newQueue
      }

    case NotificationAboutSuccessfullyAppliedModifier =>
      context.system.eventStream.publish(UpdatedHistory(history))
      if (history.isFullChainSynced) {
        logger.info(s"blockchain is synced on nvh on height ${history.getBestHeaderHeight}!")
        ModifiersCache.setChainSynced()
        Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainIsSynced)
      }
      currentNumberOfAppliedModifiers -= 1
      logger.info(s"Get NotificationAboutSuccessfullyAppliedModifier. Trying to get new one." +
        s"new currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers." +
        s" ModCahe size ${ModifiersCache.size}.")
      if (modifiersQueue.nonEmpty) {
        logger.info(s"modifiersQueue.nonEmpty in NotificationAboutSuccessfullyAppliedModifier. Sent NewModifierNotification")
        sender() ! NewModifierNotification
      } else {
        logger.info(s"modifiersQueue.isEmpty")
      }
      getModifierForApplying()

    case nonsense =>
      logger.info(s"History applicator actor got from $sender message $nonsense.")
  }

  def getModifierForApplying(): Unit = if (currentNumberOfAppliedModifiers < setting.levelDB.maxVersions) {
    logger.debug(s"It's possible to append new modifier to history. Trying to get new one from the cache.")
    val tmp: List[PersistentModifier] = ModifiersCache.popCandidate(history)
    if (tmp.isEmpty) logger.debug(s"No applicable modifier to history from cache.")
    if (tmp.size > 1) logger.debug(s"Size of getModifierForApplying tmp is ${tmp.size}")
    logger.debug(s"${ModifiersCache.cache.map(l => l._2.encodedId).mkString(",")}")
    tmp.foreach { modifier =>
      logger.debug(s"Found new modifier ${modifier.encodedId} with type ${modifier.modifierTypeId}." +
        s"currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers." +
        s" Current mod cache size ${ModifiersCache.size}")
      self ! ModifierToHistoryAppending(modifier)
      currentNumberOfAppliedModifiers += 1
    }
  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = None): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      if (tid != Transaction.modifierTypeId) logger.info(s"NVH trigger sending DownloadRequest to NVSH with type: $tid " +
        s"for modifier: ${Algos.encode(id)}. PrevMod is: ${previousModifier.map(Algos.encode)}.")
      nodeViewSynchronizer ! DownloadRequest(tid, id, previousModifier)
    }

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

}

object HistoryApplicator {

  final case class ModifierToHistoryAppending(modifier: PersistentModifier) extends AnyVal

  case object NewModifierNotification

  final case class ModifierFromNetwork(mod: PersistentModifier) extends AnyVal


  final case class StartModifiersApplicationOnStateApplicator(progressInfo: ProgressInfo,
                                                              suffixApplied: IndexedSeq[PersistentModifier])

  def props(history: History, setting: EncryAppSettings, state: UtxoState): Props =
    Props(new HistoryApplicator(history, state, setting))
}