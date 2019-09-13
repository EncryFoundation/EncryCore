package encry.view.actors

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallyFailedModification}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.ModifiersCache
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.actors.NodeViewHolder.{DownloadRequest, TransactionsForWallet}
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator._
import encry.view.actors.WalletApplicator.WalletNeedRollbackTo
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.collection.immutable.Queue
import scala.collection.mutable

class HistoryApplicator(history: History,
                        state: UtxoState,
                        wallet: EncryWallet,
                        setting: EncryAppSettings,
                        nodeViewHolder: ActorRef) extends Actor with StrictLogging {

  val walletApplicator: ActorRef =
    context.system.actorOf(WalletApplicator.props(wallet, self), "walletApplicator")

  val stateApplicator: ActorRef = context.system.actorOf(
    StateApplicator.props(setting, history, self, state, walletApplicator, nodeViewHolder)
      .withDispatcher("state-applicator-dispatcher"), name = "stateApplicator"
  )

  var modifiersQueue: Queue[(String, ProgressInfo)] = Queue.empty[(String, ProgressInfo)]
  var currentNumberOfAppliedModifiers: Int = 0
  var locallyGeneratedModifiers: Queue[PersistentModifier] = Queue.empty[PersistentModifier]

  override def receive: Receive = {
    case ModifierFromRemote(mod) if !history.isModifierDefined(mod.id) && !ModifiersCache.contains(toKey(mod.id)) =>
      ModifiersCache.put(toKey(mod.id), mod, history)
      getModifierForApplying()

    case ModifierFromRemote(modifier) =>
      logger.info(s"Modifier ${modifier.encodedId} contains in history or in modifiers cache. Reject it.")

    case LocallyGeneratedBlock(block) =>
      logger.info(s"History applicator got locally generated modifier ${block.encodedId}.")
      locallyGeneratedModifiers = locallyGeneratedModifiers.enqueue(block.header)
      locallyGeneratedModifiers = locallyGeneratedModifiers.enqueue(block.payload)

    case NeedToReportAsValid(modifier) =>
      logger.info(s"Modifier ${modifier.encodedId} should be marked as valid.")
      history.reportModifierIsValid(modifier)

    case ModifierToHistoryAppending(modifier, isLocallyGenerated) =>
      logger.info(s"Starting to apply modifier ${modifier.encodedId} to history.")
      history.append(modifier) match {
        case Left(ex) =>
          currentNumberOfAppliedModifiers -= 1
          logger.info(s"Modifier ${modifier.encodedId} unsuccessfully applied to history with exception ${ex.getMessage}." +
            s" Current currentNumberOfAppliedModifiers $currentNumberOfAppliedModifiers.")
          context.system.eventStream.publish(SyntacticallyFailedModification(modifier, List(HistoryApplyError(ex.getMessage))))
        case Right(progressInfo) if progressInfo.toApply.nonEmpty =>
          logger.info(s"Modifier ${modifier.encodedId} successfully applied to history.")
          modifiersQueue = modifiersQueue.enqueue(modifier.encodedId -> progressInfo)
          logger.info(s"New element put into queue. Current queue size is ${modifiersQueue.length}." +
            s"Current number of applied modifiers is $currentNumberOfAppliedModifiers.")
          if (progressInfo.chainSwitchingNeeded)
            walletApplicator ! WalletNeedRollbackTo(VersionTag !@@ progressInfo.branchPoint.get)
          if(progressInfo.toRemove.nonEmpty)
            nodeViewHolder ! TransactionsForWallet(progressInfo.toRemove)
          stateApplicator ! NotificationAboutNewModifier
          getModifierForApplying()
        case Right(progressInfo) =>
          logger.info(s"Progress info is empty after appending to the state.")
          if (!isLocallyGenerated) requestDownloads(progressInfo)
          context.system.eventStream.publish(SemanticallySuccessfulModifier(modifier))
          currentNumberOfAppliedModifiers -= 1
          getModifierForApplying()
      }

    case RequestNextModifier =>
      logger.info(s"Got request for the new next modifier")
      modifiersQueue.dequeueOption.foreach { case ((mod, pi), newQueue) =>
        logger.info(s"Found new valid for state modifier $mod. Send it to the state applicator.")
        sender() ! StartModifiersApplicationOnStateApplicator(pi, IndexedSeq.empty[PersistentModifier])
        modifiersQueue = newQueue
      }

    case NotificationAboutSuccessfullyAppliedModifier =>
      if (history.isFullChainSynced) {
        logger.info(s"BlockChain is synced on state applicator at height ${history.getBestHeaderHeight}!")
        ModifiersCache.setChainSynced()
        context.system.eventStream.publish(FullBlockChainIsSynced())
      }
      currentNumberOfAppliedModifiers -= 1
      logger.info(s"Get NotificationAboutSuccessfullyAppliedModifier. Trying to get new one." +
        s"new currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers." +
        s" ModCache size is ${ModifiersCache.size}.")
      if (modifiersQueue.nonEmpty) {
        logger.info(s"modifiersQueue.nonEmpty in NotificationAboutSuccessfullyAppliedModifier. Sent NewModifierNotification")
        sender() ! NotificationAboutNewModifier
      }
      getModifierForApplying()

    case NeedToReportAsInValid(block) =>
      logger.info(s"History got message NeedToReportAsInValid for block ${block.encodedId}.")
      val (_, newProgressInfo: ProgressInfo) = history.reportModifierIsInvalid(block)
      sender() ! NewProgressInfoAfterMarkingAsInValid(newProgressInfo)

    case nonsense => logger.info(s"History applicator actor got from $sender message $nonsense.")
  }

  def getModifierForApplying(): Unit = if (currentNumberOfAppliedModifiers < setting.levelDB.maxVersions) {
    logger.debug(s"It's possible to append new modifier to history. Trying to get new one from the cache.")
    if (locallyGeneratedModifiers.nonEmpty) locallyGeneratedModifiers.dequeueOption.foreach {
      case (modifier, newQueue) =>
        locallyGeneratedModifiers = newQueue
        currentNumberOfAppliedModifiers += 1
        logger.debug(s"Found new local modifier ${modifier.encodedId} with type ${modifier.modifierTypeId}." +
          s"currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers." +
          s" Current locally generated modifiers size ${locallyGeneratedModifiers.size}.")
        self ! ModifierToHistoryAppending(modifier, isLocallyGenerated = true)
    }
    else ModifiersCache.popCandidate(history).foreach { modifier =>
      currentNumberOfAppliedModifiers += 1
      logger.debug(s"Found new modifier ${modifier.encodedId} with type ${modifier.modifierTypeId}." +
        s"currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers." +
        s" Current mod cache size ${ModifiersCache.size}")
      self ! ModifierToHistoryAppending(modifier)
    }
  }

  def requestDownloads(pi: ProgressInfo): Unit = pi.toDownload.foreach { case (tid, id) =>
    if (tid != Transaction.modifierTypeId)
      logger.debug(s"HistoryApplicator call requestDownloads for modifier ${Algos.encode(id)} of type $tid")
    context.system.eventStream.publish(DownloadRequest(tid, id))
  }

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte
  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

}

object HistoryApplicator {

  case object NotificationAboutNewModifier

  final case class ModifierToHistoryAppending(modifier: PersistentModifier, isLocallyGenerated: Boolean = false)

  final case class StartModifiersApplicationOnStateApplicator(progressInfo: ProgressInfo,
                                                              suffixApplied: IndexedSeq[PersistentModifier])

  def props(history: History,
            setting: EncryAppSettings,
            state: UtxoState,
            wallet: EncryWallet,
            nodeViewHolder: ActorRef): Props =
    Props(new HistoryApplicator(history, state, wallet, setting, nodeViewHolder))
}