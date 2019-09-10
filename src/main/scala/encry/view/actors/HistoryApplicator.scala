package encry.view.actors

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.nodeViewSynchronizer
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallyFailedModification
import encry.settings.EncryAppSettings
import encry.view.ModifiersCache
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.DownloadRequest
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator.{NotificationAboutSuccessfullyAppliedModifier, RequestNextModifier}
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.collection.immutable.{HashMap, Queue}
import scala.collection.mutable

class HistoryApplicator(history: History,
                        setting: EncryAppSettings,
                        state: UtxoState) extends Actor with StrictLogging {

  val stateApplicator: ActorRef = context.system.actorOf(StateApplicator.props(setting, history, self, state)
  .withDispatcher("state-applicator-dispatcher"))

  var modifiersQueue: Queue[(PersistentModifier, ProgressInfo)] = Queue.empty[(PersistentModifier, ProgressInfo)]
  var modifiersCache: HashMap[String, PersistentModifier] = HashMap.empty[String, PersistentModifier]
  var currentNumberOfAppliedModifiers: Int = 0

  override def receive: Receive = modifiersFromNetwork
    .orElse(processingModifier)

  def modifiersFromNetwork: Receive = {
    case ModifierFromNetwork(mod) if !history.isModifierDefined(mod.id) && !ModifiersCache.contains(toKey(mod.id)) =>
      logger.info(s"Got new modifier ${mod.encodedId} of type ${mod.modifierTypeId} from network. Put it into modifiers cache.")
      //modifiersCache = modifiersCache.updated(mod.encodedId, mod)
      ModifiersCache.put(toKey(mod.id), mod, history)
      getModifierForApplying()

    case ModifierFromNetwork(mod) =>
      logger.info(s"Modifier ${mod.encodedId} contains in history or in modifiers cache. Reject it.")
  }

  def processingModifier: Receive = {
    case ModifierForHistoryApplicator(mod) =>
      logger.info(s"Starting to apply modifier ${mod.encodedId} to history.")
      history.append(mod) match {
        case Left(ex) =>
          logger.info(s"Modifier ${mod.encodedId} unsuccessfully applied to history with exception ${ex.getMessage}.")
          context.system.eventStream.publish(SyntacticallyFailedModification(mod, List(HistoryApplyError(ex.getMessage))))
        case Right(progressInfo) =>
          logger.info(s"Modifier ${mod.encodedId} successfully applied to history.")
          modifiersQueue = modifiersQueue.enqueue(mod -> progressInfo)
          logger.info(s"New element put into queue. Current queue size is ${modifiersQueue.length}." +
            s"Current number of applied modifiers is $currentNumberOfAppliedModifiers.")
          stateApplicator ! NewModifierNotification
          getModifierForApplying()
      }

    case RequestNextModifier =>
      logger.info(s"Got request for the new next modifier")
      currentNumberOfAppliedModifiers -= 1
      val nextModifier: Option[((PersistentModifier, ProgressInfo), Queue[(PersistentModifier, ProgressInfo)])] =
        modifiersQueue.dequeueOption
      nextModifier.foreach { case ((mod, pi), newQueue) =>
        logger.info(s"Found new valid for state modifier ${mod.encodedId}. Send it to the state applicator.")
        sender() ! StartModifiersApplicationOnStateApplicator(pi, IndexedSeq.empty[PersistentModifier])
        modifiersQueue = newQueue
        logger.debug(s"\nStarting updating state in updateState function!")
        pi.toApply.foreach {
          case header: Header => requestDownloads(pi, Some(header.id))
          case _ => requestDownloads(pi, None)
        }
      }

    case NotificationAboutSuccessfullyAppliedModifier =>
      logger.info(s"Get NotificationAboutSuccessfullyAppliedModifier. Trying to get new one.")
      getModifierForApplying()
  }

  def getModifierForApplying(): Unit = if (currentNumberOfAppliedModifiers < setting.levelDB.maxVersions) {
    //logger.info(s"It's possible to append new modifier to history. Trying to get new one from th cache.")
    ModifiersCache.popCandidate(history).foreach { modifier =>
      logger.info(s"Found new modifier ${modifier.encodedId} with type ${modifier.modifierTypeId}." +
        s"currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers.")
      self ! ModifierForHistoryApplicator(modifier)
      currentNumberOfAppliedModifiers += 1
      modifiersCache = modifiersCache - modifier.encodedId
    }
  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = None): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      if (tid != Transaction.modifierTypeId) logger.debug(s"NVH trigger sending DownloadRequest to NVSH with type: $tid " +
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

  def props(history: History, setting: EncryAppSettings,state: UtxoState): Props =
    Props(new HistoryApplicator(history, setting, state))
}