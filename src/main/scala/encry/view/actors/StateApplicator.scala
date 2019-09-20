package encry.view.actors

import java.io.File
import akka.actor.{Actor, ActorRef, Kill, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.consensus.EncrySupplyController
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.actors.NodeViewHolder.DownloadRequest
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator._
import encry.view.actors.TransactionsValidator.{TransactionValidatedFailure, TransactionValidatedSuccessfully}
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import cats.syntax.option._
import encry.modifiers.history.HeaderChain
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError.StateModifierApplyError
import encry.view.actors.WalletApplicator.WalletNeedScanPersistent
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.utils.Algos
import scala.collection.IndexedSeq
import scala.util.{Failure, Success, Try}

class StateApplicator(settings: EncryAppSettings,
                      historyApplicator: ActorRef,
                      walletApplicator: ActorRef,
                      nodeViewHolder: ActorRef) extends Actor with StrictLogging {

  //todo 1. add supervisor strategy

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitialInfoForStateInitialization])
    context.system.eventStream.subscribe(self, classOf[NotificationAboutNewModifier])
  }

  override def receive: Receive = initializingState

  def initializingState: Receive = {
    case InitialInfoForStateInitialization(history, ntp) =>
      val (state, historyUpdated, wallet) = initializeState(history, settings) match {
        case Some((state, wallet)) => (state, history , wallet)
        case None                  => initializeGenesisNodeState(ntp)
      }
      logger.info(s"State restored successfully. Now StateApplicator is working on full.")
      sender() ! StateApplicatorStarted(historyUpdated, state, wallet)
      walletApplicator ! NewWalletForWalletApplicator(wallet)
      context.become(updateState(state, isInProgress = false))
    case nonsense =>
      logger.info(s"Actor had received message $nonsense during state initialized.")
  }

  def updateState(state: UtxoState, isInProgress: Boolean): Receive = {
    case NotificationAboutNewModifier() if !isInProgress =>
      logger.info(s"StateApplicator got notification about new modifier and sent request for another one.")
      sender() ! RequestNextModifier
      context.become(updateState(state, isInProgress = true))

    case NotificationAboutNewModifier() =>
      logger.info(s"StateApplicator got notification about new modifier but inProgress was $isInProgress.")

    case StartModifiersApplicationOnStateApplicator(progressInfo, suffixApplied) =>
      logger.info(s"Starting applying to the state.")
      progressInfo.toApply.foreach(_ => requestDownloads(progressInfo))
      val branchPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
      val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]) =
        if (progressInfo.chainSwitchingNeeded) branchPointOpt
          .map(branchPoint =>
            if (!state.version.sameElements(branchPoint))
              state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied.toIndexedSeq, ModifierId !@@ branchPoint)
            else Success(state) -> IndexedSeq.empty[PersistentModifier])
          .getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
        else Success(state) -> suffixApplied
      stateToApplyTry match {
        case Failure(exception) =>
          context.system.eventStream.publish(RollbackFailed(branchPointOpt))
          EncryApp.forceStopApplication(500, s"Rollback failed: $exception.")
        case Success(stateToApply) =>
          logger.info(s"Successfully applied to the state. Starting modifiers applying.")
          context.system.eventStream.publish(RollbackSucceed(branchPointOpt))
          self ! StartModifiersApplying
          context.become(modifierApplication(
            progressInfo.toApply.toList,
            UpdateInformation(none, none, suffixTrimmed),
            stateToApply
          ))
      }
    case msg => logger.info(s"Got $msg in updateState")
  }

  def modifierApplication(toApply: List[PersistentModifier],
                          updateInformation: UpdateInformation,
                          currentState: UtxoState): Receive = {
    case StartModifiersApplying if updateInformation.failedMod.isEmpty => toApply.headOption.foreach {
      case header: Header =>
        historyApplicator ! NeedToReportAsValid(header)
        context.system.eventStream.publish(SemanticallySuccessfulModifier(header))
        val newToApply: List[PersistentModifier] = toApply.drop(1)
        if (newToApply.nonEmpty) {
          logger.info(s"Header ${header.encodedId} with height ${header.height} in receive modifierApplication" +
            s" applied successfully. Starting new modifier application.")
          self ! StartModifiersApplying
          context.become(modifierApplication(
            newToApply,
            UpdateInformation(none, none, updateInformation.suffix :+ header),
            currentState.copy(lastBlockTimestamp = header.timestamp)
          ))
        } else {
          logger.info(s"Finished modifiers application. Become to modifiersApplicationCompleted." +
            s"Headers height is ${header.height}")
          self ! ModifiersApplicationFinished
          context.become(modifiersApplicationCompleted(
            UpdateInformation(none, none, updateInformation.suffix :+ header),
            currentState
          ))
        }
      case block: Block =>
        logger.info(s"Start block ${block.encodedId} validation to the state with height ${block.header.height}")
        val lastTxId: ModifierId = block.payload.txs.last.id
        val totalFees: Amount = block.payload.txs.dropRight(1).map(_.fee).sum
        block.payload.txs.foreach {
          case tx if tx.id sameElements lastTxId => context.actorOf(
            TransactionsValidator.props(
              currentState, totalFees + EncrySupplyController.supplyAt(
                currentState.height, settings.constants.InitialEmissionAmount,
                settings.constants.EmissionEpochLength, settings.constants.EmissionDecay
              ), tx, currentState.height)
              .withDispatcher("transaction-validator-dispatcher"),
            s"validatorFor:${tx.encodedId}"
          )
          case tx => context.actorOf(
            TransactionsValidator.props(currentState, 0L, tx, currentState.height)
              .withDispatcher("transaction-validator-dispatcher"),
            s"validatorFor:${tx.encodedId}"
          )
        }
        context.become(awaitingTransactionsValidation(
          toApply.drop(1), updateInformation, block, currentState, block.payload.txs.length
        ))
    }
    case StartModifiersApplying => //todo redundant iterations while updateInformation.failedMod.nonEmpty
      val newToApply: List[PersistentModifier] = toApply.drop(1)
      if (newToApply.nonEmpty) {
        self ! StartModifiersApplying
        logger.info(s"StartModifiersApplying updateInformation.failedMod.nonEmpty")
        context.become(modifierApplication(newToApply, updateInformation, currentState))
      } else {
        logger.info(s"StartModifiersApplying w/0 if case to apply is empty")
        self ! ModifiersApplicationFinished
        context.become(modifiersApplicationCompleted(
          UpdateInformation(none, none, updateInformation.suffix),
          currentState
        ))
      }
    case msg => logger.info(s"Got $msg in modifierApplication")
  }

  def modifiersApplicationCompleted(ui: UpdateInformation, currentState: UtxoState): Receive = {
    case ModifiersApplicationFinished => ui.failedMod match {
      case Some(_) =>
        logger.info(s"sent to self StartModifiersApplicationOnStateApplicator. ui.alternativeProgressInfo.get" +
          s" ${ui.alternativeProgressInfo.get.toApply.isEmpty}")
        self ! StartModifiersApplicationOnStateApplicator(ui.alternativeProgressInfo.get, ui.suffix)
        context.become(updateState(currentState, isInProgress = true))
      case _ =>
        logger.info(s"Send request for the next modifier to the history applicator")
        historyApplicator ! NotificationAboutSuccessfullyAppliedModifier
        if (ui.suffix.nonEmpty) walletApplicator ! WalletNeedScanPersistent(ui.suffix)
        nodeViewHolder ! StateForNVH(currentState)
        context.become(updateState(currentState, isInProgress = false))
    }
    case msg => logger.info(s"Got $msg in modifiersApplicationCompleted")
  }

  def awaitingRollbackHistory(state: UtxoState): Receive = {
    case _ =>
  }

  def awaitingTransactionsValidation(toApply: Seq[PersistentModifier],
                                     ui: UpdateInformation,
                                     block: Block,
                                     currentState: UtxoState,
                                     numberOfTransactions: Int): Receive = {
    case TransactionValidatedSuccessfully =>
      if (numberOfTransactions - 1 == 0) {
        val prevVersion: VersionTag = currentState.version
        val updatedState: UtxoState = currentState.applyValidModifier(block)
        /* */
        if (!(updatedState.storage.root sameElements block.header.stateRoot)) {
          //rollback state
          //updatedState.rollbackTo(prevVersion) match {
          //  case Failure(exception) => //stop app
          //  case Success(rolledBackState) =>
          //    historyApplicator ! RollbackHistoryToVersion(prevVersion, block)
          //    context.become(awaitingRollbackHistory(rolledBackState))
          // }
          historyApplicator ! NeedToReportAsInValid(block)
          context.system.eventStream.publish(
            SemanticallyFailedModification(block, List(StateModifierApplyError(s"Invalid modifier by state")))
          )
          context.become(awaitingNewProgressInfo(block, ui, toApply, updatedState))
        } else {
          //all is ok
          context.system.eventStream.publish(SemanticallySuccessfulModifier(block))
          historyApplicator ! NeedToReportAsValid(block)
          if (toApply.isEmpty) {
            logger.info(s"Finished modifiers application. Become to modifiersApplicationCompleted.")
            self ! ModifiersApplicationFinished
            context.become(modifiersApplicationCompleted(ui, updatedState))
          } else {
            logger.info(s"awaitingTransactionsValidation finished but infoAboutCurrentFoldIteration._1 is not empty.")
            self ! StartModifiersApplying
            context.become(modifierApplication(toApply.toList, ui, updatedState))
          }
        }
      } else context.become(awaitingTransactionsValidation(toApply, ui, block, currentState, numberOfTransactions - 1))

    case TransactionValidatedFailure(tx, ex) =>
      logger.info(s"Transaction ${tx.encodedId} failed in validation by state.")
      context.children.foreach(_ ! Kill)
      context.system.eventStream.publish(SemanticallyFailedModification(block, List(StateModifierApplyError(s"$ex"))))
      historyApplicator ! NeedToReportAsInValid(block)
      context.become(awaitingNewProgressInfo(block, ui, toApply, currentState))

    case msg => logger.info(s"Got $msg in awaitingTransactionsValidation")
  }

  def awaitingNewProgressInfo(block: Block,
                              ui: UpdateInformation,
                              toApply: Seq[PersistentModifier],
                              currentState: UtxoState): Receive = {
    case NewProgressInfoAfterMarkingAsInValid(pi) =>
      self ! StartModifiersApplying
      context.become(
        modifierApplication(
          toApply.toList,
          UpdateInformation(block.some, pi.some, ui.suffix),
          currentState
        )
      )
    case msg => logger.info(s"Got $msg in awaitingNewProgressInfo")
  }

  def trimChainSuffix(suffix: IndexedSeq[PersistentModifier],
                      rollbackPoint: ModifierId): IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq.empty else suffix.drop(idx)
  }

  def requestDownloads(pi: ProgressInfo): Unit = pi.toDownload.foreach { case (tid, id) =>
    if (tid != Transaction.modifierTypeId)
      logger.debug(s"StateApplicator call requestDownloads for modifier ${Algos.encode(id)} of type $tid")
    context.system.eventStream.publish(DownloadRequest(tid, id))
  }

  def initializeState(history: History, settings: EncryAppSettings): Option[(UtxoState, EncryWallet)] =
    if (History.getHistoryIndexDir(settings).listFiles.nonEmpty)
      try {
        val stateDir: File = UtxoState.getStateDir(settings)
        stateDir.mkdirs()
        val consistentState: UtxoState = restoreConsistentState(UtxoState.create(stateDir, settings), history)
        val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
        logger.info(s"State initialized successfully.")
        (consistentState -> wallet).some
      } catch {
        case ex: Throwable => logger.info(s"During state initialisation error $ex has thrown."); none
      }
    else none

  def initializeGenesisNodeState(timeProvider: NetworkTimeProvider): (UtxoState, History, EncryWallet) = {
    new File(settings.directory).listFiles.foreach(FileUtils.cleanDirectory)
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty.")
    val state: UtxoState = UtxoState.genesis(stateDir, settings)
    val history: History = History.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    logger.info(s"After inconsistent start, genesis state initialized successfully.")
    (state, history, wallet)
  }

  def restoreConsistentState(stateIn: UtxoState, history: History): UtxoState =
    (stateIn.version, history.getBestBlock) match {
      case (stateId, None) if stateId sameElements Array.emptyByteArray =>
        logger.info(s"State and history are both empty on startup."); stateIn
      case (stateId, Some(block)) if stateId sameElements block.id =>
        logger.info(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed."); stateIn
      case (_, None) =>
        logger.info(s"History is empty, creating genesis state.")
        stateIn.storage.close()
        emptyState(settings)
      case (stateId, Some(historyBestBlock)) =>
        val (rollbackId: Option[ModifierId], newChain: HeaderChain) =
          history.getChainToHeader(history.getHeaderById(ModifierId !@@ stateId), historyBestBlock.header)
        logger.info(s"State and history have different versions. Going to rollback state to " +
          s"${rollbackId.map(Algos.encode)} version and apply ${newChain.length} modifiers.")
        val startState: UtxoState = rollbackId
          .flatMap(id => stateIn.rollbackTo(VersionTag !@@ id).toOption)
          .getOrElse(emptyState(settings))
        val toApply: IndexedSeq[Block] = newChain.headers.map(h => history.getBlockByHeader(h) match {
          case Some(fb) => fb
          case None => throw new Exception(s"Failed to get full block for header $h.")
        })
        toApply.foldLeft(startState) { (s, m) => s.applyModifierWhileStateInitializing(m).right.get }
    }

  def emptyState(settings: EncryAppSettings): UtxoState = {
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    logger.info(s"Empty state created.")
    UtxoState.create(stateDir, settings)
  }
}

object StateApplicator {

  final case class RollbackHistoryToVersion(version: VersionTag, failedBlock: Block)

  final case class StateApplicatorStarted(history: History, state: UtxoState, wallet: EncryWallet)

  final case class StateRestoredSuccessfully(wallet: EncryWallet, state: UtxoState)

  final case class GenesisHistoryForHistoryApplicator(history: History, wallet: EncryWallet, state: UtxoState)

  final case class NewWalletForWalletApplicator(wallet: EncryWallet) extends AnyVal

  final case class StateForNVH(state: UtxoState) extends AnyVal

  final case class NeedToReportAsValid(modifier: PersistentModifier) extends AnyVal

  final case class NeedToReportAsInValid(modifier: PersistentModifier) extends AnyVal

  final case class NewProgressInfoAfterMarkingAsInValid(pi: ProgressInfo) extends AnyVal

  case object NotificationAboutSuccessfullyAppliedModifier

  case object StartModifiersApplying

  case object ModifiersApplicationFinished

  case object RequestNextModifier

  final case class UpdateInformation(failedMod: Option[PersistentModifier],
                                     alternativeProgressInfo: Option[ProgressInfo],
                                     suffix: IndexedSeq[PersistentModifier])

  def props(setting: EncryAppSettings,
            historyApplicator: ActorRef,
            walletApplicator: ActorRef,
            nodeViewHolder: ActorRef): Props =
    Props(new StateApplicator(setting, historyApplicator, walletApplicator, nodeViewHolder))
}