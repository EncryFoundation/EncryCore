package encry.view.actors

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.local.miner.Miner.{CandidateEnvelope, NewCandidate, StartProducingNewCandidate, WrongConditionsForNewBlock}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.{ModifierAppendedToHistory, ModifierAppendedToState, TransactionsInBlock}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.ModifiersCache
import encry.view.NodeViewErrors.ModifierApplyError.{HistoryApplyError, StateModifierApplyError}
import encry.view.actors.NodeViewHolder.{DownloadRequest, TransactionsForWallet}
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator._
import encry.view.actors.WalletApplicator.WalletNeedRollbackTo
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.concurrent.duration._

class HistoryApplicator(nodeViewHolder: ActorRef,
                        walletApplicator: ActorRef,
                        stateApplicator: ActorRef,
                        settings: EncryAppSettings,
                        influxRef: Option[ActorRef],
                        dataHolder: ActorRef,
                        networkTimeProvider: NetworkTimeProvider) extends Actor with StrictLogging {

  //todo 1. add history.close in postStop

  import context.dispatcher

  var modifiersQueue: Queue[(PersistentModifier, ProgressInfo)] = Queue.empty[(PersistentModifier, ProgressInfo)]
  var currentNumberOfAppliedModifiers: Int = 0
  var locallyGeneratedModifiers: Queue[PersistentModifier] = Queue.empty[PersistentModifier]

  override def preStart(): Unit = {
    self ! HistoryInitializedSuccessfully
  }

  override def receive: Receive = initializeHistory(restoreHistory)

  def initializeHistory(history: History): Receive = {
    case HistoryInitializedSuccessfully =>
      logger.info(s"History initialized successfully. Published history link.")
      stateApplicator ! InitialInfoForStateInitialization(history, self, networkTimeProvider)
    case StateApplicatorStarted(historyUpdated, state, wallet) =>
      logger.info(s"History applicator got confirmation that state initialized successfully.")
      nodeViewHolder ! InitialStateHistoryWallet(historyUpdated, wallet, state)
      context.become(mainBehaviour(historyUpdated))
  }

  def mainBehaviour(history: History): Receive = {
    case GetHistory => sender() ! history
    case msg@StartProducingNewCandidate(_, _, _) if (history.isFullChainSynced && history.isBestBlockDefined)
                                                                               || settings.node.offlineGeneration =>
      logger.info(s"History applicator got StartProducingNewCandidate. history.isFullChainSynced = ${history.isFullChainSynced}" +
        s" history.isBestBlockDefined = ${history.isBestBlockDefined}. settings.node.offlineGeneration = ${settings.node.offlineGeneration}.")
      val header: Option[Header] = history.getHeaderOfBestBlock
      stateApplicator ! msg.copy(
        bestHeaderOpt = header,
        difficulty = header.map(history.requiredDifficultyAfter(_) match {
          case Left(value)  => logger.info(s"$value"); sys.exit(999)
          case Right(value) => value
        }) getOrElse settings.constants.InitialDifficulty
      )
    case StartProducingNewCandidate(txs, _, _) =>
      logger.info(s"History applicator got StartProducingNewCandidate with out if condition. history.isFullChainSynced = ${history.isFullChainSynced}" +
        s" history.isBestBlockDefined = ${history.isBestBlockDefined}. settings.node.offlineGeneration = ${settings.node.offlineGeneration}.")
      sender() ! WrongConditionsForNewBlock(txs)
    case msg@NewCandidate(_, _, _, _, _) =>
      logger.info(s"History applicator got NewCandidate. Send to NVH")
      nodeViewHolder ! msg

    case ModifierFromRemote(mod) if !history.isModifierDefined(mod.id) && !ModifiersCache.contains(toKey(mod.id)) =>
      ModifiersCache.put(toKey(mod.id), mod, history)
      getModifierForApplying(history)

    case ModifierFromRemote(modifier) =>
      logger.info(s"Modifier ${modifier.encodedId} contains in history or in modifier's cache. It was rejected.")

    case ModifierToHistoryAppending(modifier, isLocallyGenerated) =>
      logger.info(s"Modifier ${modifier.encodedId} application to history has been starting.")
      history.append(modifier) match {
        case Left(ex) =>
          currentNumberOfAppliedModifiers -= 1
          logger.info(s"Modifier ${modifier.encodedId} unsuccessfully applied to history with exception ${ex.getMessage}." +
            s" Current number of applied modifiers is $currentNumberOfAppliedModifiers.")
          context.system.eventStream.publish(
            SyntacticallyFailedModification(modifier, List(HistoryApplyError(ex.getMessage)))
          )
        case Right(progressInfo) if progressInfo.toApply.nonEmpty =>
          logger.info(s"Modifier ${modifier.encodedId} successfully applied to history.")
          modifiersQueue = modifiersQueue.enqueue(modifier -> progressInfo)
          logger.info(s"New element put into queue. Current queue size is ${modifiersQueue.length}." +
            s"Current number of applied modifiers is $currentNumberOfAppliedModifiers.")
          influxRef.foreach(_ ! ModifierAppendedToHistory(modifier match {
            case _: Header => true
            case _: Payload => false
          }, success = true))
          if (progressInfo.chainSwitchingNeeded)
            walletApplicator ! WalletNeedRollbackTo(VersionTag !@@ progressInfo.branchPoint.get)
          if (progressInfo.toRemove.nonEmpty)
            nodeViewHolder ! TransactionsForWallet(progressInfo.toRemove)
          stateApplicator ! NotificationAboutNewModifier
          getModifierForApplying(history)
          dataHolder ! ChangedHistory(history)
        case Right(progressInfo) =>
          logger.info(s"Progress info is empty after appending to the state.")
          if (!isLocallyGenerated) requestDownloads(progressInfo)
          dataHolder ! ChangedHistory(history)
          context.system.eventStream.publish(SemanticallySuccessfulModifier(modifier))
          currentNumberOfAppliedModifiers -= 1
          getModifierForApplying(history)
      }

    case RequestNextModifier =>
      logger.info(s"Got request for the new next modifier from state applicator.")
      modifiersQueue.dequeueOption.foreach { case ((mod, pi), newQueue) =>
        logger.info(s"Found new valid for state modifier ${mod.encodedId}. Send it to the state applicator.")
        sender() ! StartModifiersApplicationOnStateApplicator(pi, IndexedSeq.empty[PersistentModifier])
        modifiersQueue = newQueue
      }

    case LocallyGeneratedBlock(block) =>
      logger.info(s"History applicator got self mined block ${block.encodedId}.")
      locallyGeneratedModifiers = locallyGeneratedModifiers.enqueue(block.header)
      locallyGeneratedModifiers = locallyGeneratedModifiers.enqueue(block.payload)
      getModifierForApplying(history)

    case NeedToReportAsValid(modifier) =>
      logger.info(s"Modifier ${modifier.encodedId} should be marked as valid.")
      history.reportModifierIsValid(modifier)
      influxRef.foreach { ref =>
        ref ! ModifierAppendedToState(success = true)
        modifier match {
          case Block(_, payload) if history.isFullChainSynced => ref ! TransactionsInBlock(payload.txs.size)
          case _ => //do nothing
        }
      }

    case NeedToReportAsInValid(block) =>
      logger.info(s"History got message NeedToReportAsInValid for block ${block.encodedId}.")
      currentNumberOfAppliedModifiers -= 1
      val (_, newProgressInfo: ProgressInfo) = history.reportModifierIsInvalid(block)
      sender() ! NewProgressInfoAfterMarkingAsInValid(newProgressInfo)

    case NotificationAboutSuccessfullyAppliedModifier =>
      if (history.isFullChainSynced) {
        logger.info(s"Block chain is synced at height ${history.getBestHeaderHeight}.")
        ModifiersCache.setChainSynced()
        context.system.eventStream.publish(FullBlockChainIsSynced())
      }
      currentNumberOfAppliedModifiers -= 1
      logger.info(s"Get NotificationAboutSuccessfullyAppliedModifier. Trying to get new one." +
        s" New currentNumberOfAppliedModifiers is $currentNumberOfAppliedModifiers." +
        s" ModCache size is ${ModifiersCache.size}.")
      if (modifiersQueue.nonEmpty) {
        logger.info(s"modifiersQueue.nonEmpty in NotificationAboutSuccessfullyAppliedModifier. Sent NewModifierNotification")
        sender() ! NotificationAboutNewModifier
      }
      getModifierForApplying(history)

    case nonsense => logger.info(s"History applicator actor got from $sender message $nonsense.")
  }

  def getModifierForApplying(history: History): Unit =
    if (currentNumberOfAppliedModifiers < settings.levelDB.maxVersions) {
      logger.debug(s"Now it's possible to append new modifier to history. Trying to get new one from the cache.")
      if (locallyGeneratedModifiers.nonEmpty) locallyGeneratedModifiers.dequeueOption.foreach {
        case (modifier, newQueue) =>
          locallyGeneratedModifiers = newQueue
          currentNumberOfAppliedModifiers += 1
          logger.debug(s"Found new self mined modifier ${modifier.encodedId} with type ${modifier.modifierTypeId}." +
            s" Current number of applied modifiers is $currentNumberOfAppliedModifiers." +
            s" Current number of self mined modifiers is ${locallyGeneratedModifiers.size}.")
          self ! ModifierToHistoryAppending(modifier, isLocallyGenerated = true)
      }
      else ModifiersCache.popCandidate(history).foreach { modifier =>
        currentNumberOfAppliedModifiers += 1
        logger.debug(s"Found new modifier ${modifier.encodedId} with type ${modifier.modifierTypeId}." +
          s" Current number of applied modifiers is $currentNumberOfAppliedModifiers." +
          s" Current modifier's cache size is ${ModifiersCache.size}.")
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

  def restoreHistory: History = History.readOrGenerate(settings, networkTimeProvider)
}

object HistoryApplicator {

  final case class InitialInfoForStateInitialization(history: History, historyAppl: ActorRef, networkTimeProvider: NetworkTimeProvider)

  case object HistoryInitializedSuccessfully

  case object GetHistory

  final case object NotificationAboutNewModifier

  final case class InitialStateHistoryWallet(history: History, wallet: EncryWallet, state: UtxoState)

  final case class ModifierToHistoryAppending(modifier: PersistentModifier, isLocallyGenerated: Boolean = false)

  final case class StartModifiersApplicationOnStateApplicator(progressInfo: ProgressInfo,
                                                              suffixApplied: IndexedSeq[PersistentModifier])

  class HistoryApplicatorPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case NeedToReportAsValid(_) | NeedToReportAsInValid(_) => 0
        case ModifierToHistoryAppending(_, _) => 1
        case NotificationAboutSuccessfullyAppliedModifier => 2
        case RequestNextModifier => 3
        case PoisonPill => 5
        case otherwise => 4
      })

  def props(nodeViewHolder: ActorRef,
            walletApplicator: ActorRef,
            stateRef: ActorRef,
            settings: EncryAppSettings,
            influxRef: Option[ActorRef],
            dataHolder: ActorRef,
            networkTimeProvider: NetworkTimeProvider): Props =
    Props(
      new HistoryApplicator(
        nodeViewHolder,
        walletApplicator,
        stateRef,
        settings,
        influxRef,
        dataHolder,
        networkTimeProvider
      ))
}