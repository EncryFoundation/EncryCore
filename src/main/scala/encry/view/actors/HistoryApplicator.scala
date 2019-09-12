package encry.view.actors

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props, Terminated}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.{miner, nodeViewSynchronizer}
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.local.miner.Miner
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.{RequestFromLocal, SemanticallySuccessfulModifier, SyntacticallyFailedModification, UpdatedHistory}
import encry.settings.EncryAppSettings
import encry.view.ModifiersCache
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.DownloadRequest
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, CompareViewsWithSender}
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator.{NeedToReportValid, NotificationAboutSuccessfullyAppliedModifier, RequestNextModifier}
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.concurrent.duration._
import scala.collection.immutable.{HashMap, Queue}
import scala.collection.{Seq, mutable}

class HistoryApplicator(history: History, //try to use state monad hear
                        state: UtxoState, //try to use state monad hear
                        setting: EncryAppSettings) extends Actor with StrictLogging {

  val stateApplicator: ActorRef = context.system
    .actorOf(StateApplicator.props(setting, history, self, state).withDispatcher("state-applicator-dispatcher"),
      "state")

  var modifiersQueue: Queue[(PersistentModifier, ProgressInfo)] = Queue.empty[(PersistentModifier, ProgressInfo)]
  var modifiersCache: HashMap[String, PersistentModifier] = HashMap.empty[String, PersistentModifier]
  var currentNumberOfAppliedModifiers: Int = 0

  override def receive: Receive = modifiersFromNetwork
    .orElse(processingModifier)

  def modifiersFromNetwork: Receive = {
    case ModifierFromNetwork(mod) if !history.isModifierDefined(mod.id) && !ModifiersCache.contains(toKey(mod.id)) =>
      logger.debug(s"Got new modifier ${mod.encodedId} of type ${mod.modifierTypeId} from network." +
        s" Put it into modifiers cache.")
      ModifiersCache.put(toKey(mod.id), mod, history)
      getModifierForApplying()

    case ModifierFromNetwork(mod) =>
      logger.info(s"Modifier ${mod.encodedId} contains in history or in modifiers cache. Reject it.")

    case NeedToReportValid(h) => history.reportModifierIsValid(h)

    case CompareViewsWithSender(peer, modifierTypeId, modifierIds, senderRef) =>
      val ids: Seq[ModifierId] = modifierTypeId match {
        case _ => modifierIds
          .filterNot(mid => history.isModifierDefined(mid) || ModifiersCache.contains(toKey(mid)))
      }
      if (ids.nonEmpty && (modifierTypeId == Header.modifierTypeId || (history.isHeadersChainSynced && modifierTypeId == Payload.modifierTypeId))) {
        senderRef ! RequestFromLocal(peer, modifierTypeId, ids)
        logger.info(s"Trigger CompareViewsWithSender to $senderRef with ${ids.size}")
      }
  }

  def processingModifier: Receive = {
    case ModifierForHistoryApplicator(mod) =>
      logger.info(s"Starting to apply modifier ${mod.encodedId} to history.")
      history.append(mod) match {
        case Left(ex) =>
          currentNumberOfAppliedModifiers -= 1
          logger.info(s"Modifier ${mod.encodedId} unsuccessfully applied to history with exception ${ex.getMessage}." +
            s" Current currentNumberOfAppliedModifiers $currentNumberOfAppliedModifiers.")
          context.system.eventStream.publish(SyntacticallyFailedModification(mod, List(HistoryApplyError(ex.getMessage))))
        //todo add behaviour in failed case
        case Right(progressInfo) if progressInfo.toApply.nonEmpty =>
          logger.info(s"Modifier ${mod.encodedId} successfully applied to history.")
          modifiersQueue = modifiersQueue.enqueue(mod -> progressInfo)
          logger.info(s"New element put into queue. Current queue size is ${modifiersQueue.length}." +
            s"Current number of applied modifiers is $currentNumberOfAppliedModifiers.")
          stateApplicator ! NewModifierNotification
          getModifierForApplying()
        case Right(progressInfo) =>
          logger.info(s"Progress info is empty")
          requestDownloads(progressInfo, Some(mod.id))
          //history.reportModifierIsValid(mod)
          context.system.eventStream.publish(SemanticallySuccessfulModifier(mod))
          getModifierForApplying()
          currentNumberOfAppliedModifiers -= 1
      }

    case RequestNextModifier =>
      logger.info(s"Got request for the new next modifier")
      val nextModifier: Option[((PersistentModifier, ProgressInfo), Queue[(PersistentModifier, ProgressInfo)])] =
        modifiersQueue.dequeueOption
      nextModifier.foreach { case ((mod, pi), newQueue) =>
        logger.info(s"Found new valid for state modifier ${mod.encodedId}. Send it to the state applicator.")
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

    case Terminated =>
      logger.info(s"$sender() dropped")
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
      self ! ModifierForHistoryApplicator(modifier)
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

  case object NewModifierNotification

  final case class ModifierFromNetwork(mod: PersistentModifier) extends AnyVal

  final case class ModifierForHistoryApplicator(modifier: PersistentModifier) extends AnyVal

  final case class StartModifiersApplicationOnStateApplicator(progressInfo: ProgressInfo,
                                                              suffixApplied: IndexedSeq[PersistentModifier])

  def props(history: History, setting: EncryAppSettings, state: UtxoState): Props =
    Props(new HistoryApplicator(history, state, setting))
}