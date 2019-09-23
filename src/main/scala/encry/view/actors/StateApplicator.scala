package encry.view.actors

import akka.actor.{Actor, ActorRef, Kill, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.consensus.EncrySupplyController
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.StorageVersion
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.implicits.UTXO._
import encry.view.actors.NodeViewHolder.DownloadRequest
import encry.view.actors.HistoryApplicator.{NotificationAboutNewModifier, StartModifiersApplicationOnStateApplicator}
import encry.view.actors.StateApplicator._
import encry.view.actors.TransactionsValidator.{TransactionValidatedFailure, TransactionValidatedSuccessfully}
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId}
import cats.syntax.option._
import encry.stats.StatsSender.{ModifierAppendedToState, TransactionsInBlock}
import encry.view.NodeViewErrors.ModifierApplyError.StateModifierApplyError
import encry.view.actors.WalletApplicator.WalletNeedScanPersistent
import org.encryfoundation.common.utils.Algos
import scala.collection.IndexedSeq
import scala.util.{Failure, Success, Try}

class StateApplicator(settings: EncryAppSettings,
                      history: History,
                      historyApplicator: ActorRef,
                      state: UtxoState,
                      walletApplicator: ActorRef,
                      influxRef: Option[ActorRef]) extends Actor with StrictLogging {

  var transactionsValidatorsNumber: Int = 0
  var isInProgress: Boolean = false

  //todo add supervisor strategy

  def modifierApplication(toApply: List[PersistentModifier], updateInformation: UpdateInformation): Receive = {
    case StartModifiersApplying if updateInformation.failedMod.isEmpty =>
      toApply.headOption.foreach {
        case header: Header =>
          state.lastBlockTimestamp = header.timestamp
          historyApplicator ! NeedToReportAsValid(header)
          context.system.eventStream.publish(SemanticallySuccessfulModifier(header))
          val newToApply: List[PersistentModifier] = toApply.drop(1)
          if (newToApply.nonEmpty) {
            logger.info(s"Header ${header.encodedId} with height ${header.height} in receive modifierApplication" +
              s" applied successfully. Starting new modifier application.")
            self ! StartModifiersApplying
            context.become(modifierApplication(
              newToApply,
              UpdateInformation(none, none, updateInformation.suffix :+ header)
            ))
          } else {
            logger.info(s"Finished modifiers application. Become to modifiersApplicationCompleted." +
              s"Headers height is ${header.height}")
            self ! ModifiersApplicationFinished
            context.become(modifiersApplicationCompleted(
              UpdateInformation(none, none, updateInformation.suffix :+ header)
            ))
          }
        case block: Block =>
          logger.info(s"Start block ${block.encodedId} validation to the state with height ${block.header.height}")
          val lastTxId: ModifierId = block.payload.txs.last.id
          val totalFees: Amount = block.payload.txs.dropRight(1).map(_.fee).sum
          block.payload.txs.foreach {
            case tx if tx.id sameElements lastTxId => context.actorOf(
              TransactionsValidator.props(
                state,
                totalFees + EncrySupplyController.supplyAt(state.height, settings.constants.InitialEmissionAmount,
                  settings.constants.EmissionEpochLength, settings.constants.EmissionDecay),
                tx, state.height)
                .withDispatcher("transaction-validator-dispatcher"),
              s"validatorFor:${tx.encodedId}"
            )
              transactionsValidatorsNumber += 1
            case tx => context.actorOf(
              TransactionsValidator.props(state, 0L, tx, state.height).withDispatcher("transaction-validator-dispatcher"),
              s"validatorFor:${tx.encodedId}"
            )
              transactionsValidatorsNumber += 1
          }
          context.become(awaitingTransactionsValidation(toApply.drop(1), updateInformation, block, block.payload.txs))
      }
    case StartModifiersApplying => //todo redundant iterations while updateInformation.failedMod.nonEmpty
      val newToApply: List[PersistentModifier] = toApply.drop(1)
      if (newToApply.nonEmpty) {
        self ! StartModifiersApplying
        logger.info(s"StartModifiersApplying updateInformation.failedMod.nonEmpty")
        context.become(modifierApplication(newToApply, updateInformation))
      } else {
        logger.info(s"StartModifiersApplying w/0 if case to apply is empty")
        self ! ModifiersApplicationFinished
        context.become(modifiersApplicationCompleted(
          UpdateInformation(none, none, updateInformation.suffix)
        ))
      }
    case msg => logger.info(s"Got $msg in modifierApplication")
  }

  def modifiersApplicationCompleted(ui: UpdateInformation): Receive = {
    case ModifiersApplicationFinished => ui.failedMod match {
      case Some(_) =>
        logger.info(s"sent to self StartModifiersApplicationOnStateApplicator. ui.alternativeProgressInfo.get" +
          s" ${ui.alternativeProgressInfo.get.toApply.isEmpty}")
        self ! StartModifiersApplicationOnStateApplicator(ui.alternativeProgressInfo.get, ui.suffix)
        context.become(updateState)
      case _ =>
        logger.info(s"Send request for the next modifier to the history applicator")
        historyApplicator ! NotificationAboutSuccessfullyAppliedModifier
        if (ui.suffix.nonEmpty) walletApplicator ! WalletNeedScanPersistent(ui.suffix)
        isInProgress = false
        context.become(updateState)
    }
    case msg => logger.info(s"Got $msg in modifiersApplicationCompleted")
  }

  def awaitingTransactionsValidation(toApply: Seq[PersistentModifier],
                                     ui: UpdateInformation,
                                     block: Block,
                                     transactions: Seq[Transaction]): Receive = {
    case TransactionValidatedSuccessfully =>
      println("TransactionValidatedSuccessfully")
      transactionsValidatorsNumber -= 1
      if (transactionsValidatorsNumber == 0) {
        val combinedStateChange = combineAll(transactions.toList.map(UtxoState.tx2StateChange))
        state.storage.insert(
          StorageVersion !@@ block.id,
          combinedStateChange.outputsToDb.toList,
          combinedStateChange.inputsToDb.toList
        )
        context.system.eventStream.publish(SemanticallySuccessfulModifier(block))
        historyApplicator ! NeedToReportAsValid(block)
        influxRef.foreach { ref =>
          ref ! ModifierAppendedToState(success = true)
          if (history.isFullChainSynced) ref ! TransactionsInBlock(block.payload.txs.size)
        }
        state.height = Height @@ block.header.height
        state.lastBlockTimestamp = block.header.timestamp
        if (toApply.isEmpty) {
          logger.info(s"Finished modifiers application. Become to modifiersApplicationCompleted.")
          self ! ModifiersApplicationFinished
          context.become(modifiersApplicationCompleted(ui))
        } else {
          logger.info(s"awaitingTransactionsValidation finished but infoAboutCurrentFoldIteration._1 is not empty.")
          self ! StartModifiersApplying
          context.become(modifierApplication(toApply.toList, ui))
        }
      }

    case TransactionValidatedFailure(tx, ex) =>
      println("TransactionValidatedFailure")
      logger.info(s"Transaction ${tx.encodedId} failed in validation by state.")
      context.children.foreach(_ ! Kill)
      context.system.eventStream.publish(SemanticallyFailedModification(block, List(StateModifierApplyError(s"$ex"))))
      historyApplicator ! NeedToReportAsInValid(block)
      context.become(awaitingNewProgressInfo(block, ui, toApply))

    case msg => logger.info(s"Got $msg in awaitingTransactionsValidation")
  }

  def awaitingNewProgressInfo(block: Block, ui: UpdateInformation, toApply: Seq[PersistentModifier]): Receive = {
    case NewProgressInfoAfterMarkingAsInValid(pi) =>
      self ! StartModifiersApplying
      context.become(
        modifierApplication(
          toApply.toList,
          UpdateInformation(block.some, pi.some, ui.suffix)
        )
      )
    case msg => logger.info(s"Got $msg in awaitingNewProgressInfo")
  }

  def updateState: Receive = {
    case NotificationAboutNewModifier if !isInProgress =>
      logger.info(s"StateApplicator got notification about new modifier. Send request for new one.")
      sender() ! RequestNextModifier
      isInProgress = true

    case NotificationAboutNewModifier =>
      logger.info(s"StateApplicator got notification about new modifier but inProgress is $isInProgress.")

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
            UpdateInformation(none, none, suffixTrimmed)
          ))
      }
    case msg => logger.info(s"Got $msg in updateState")
  }

  override def receive: Receive = updateState

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
}

object StateApplicator {

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
            history: History,
            historyApplicator: ActorRef,
            state: UtxoState,
            walletApplicator: ActorRef,
            influxRef: Option[ActorRef]): Props =
    Props(new StateApplicator(setting, history, historyApplicator, state, walletApplicator, influxRef))
}