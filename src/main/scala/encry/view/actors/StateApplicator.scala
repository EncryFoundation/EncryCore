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
import encry.view.NodeViewHolder.UpdateInformation
import encry.view.actors.HistoryApplicator.{NewModifierNotification, StartModifiersApplicationOnStateApplicator}
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
import encry.view.NodeViewErrors.ModifierApplyError.StateModifierApplyError
import scala.collection.IndexedSeq
import scala.collection.immutable.HashMap
import scala.util.{Failure, Success, Try}

class StateApplicator(setting: EncryAppSettings,
                      history: History,
                      historyApplicator: ActorRef,
                      state: UtxoState) extends Actor with StrictLogging {

  var validatorsQueue: HashMap[String, ActorRef] = HashMap.empty[String, ActorRef]

  var validatedTxs: List[Transaction] = List.empty

  var infoAboutCurrentFoldIteration: (Seq[PersistentModifier], UpdateInformation, Option[Block]) =
    (Seq.empty, UpdateInformation(history, state, None, None, IndexedSeq.empty), None)

  var isInProgress: Boolean = false

  //todo add supervisor strategy

  def modifierApplication(toApply: List[PersistentModifier], updateInformation: UpdateInformation): Receive = {
    case StartModifiersApplying if updateInformation.failedMod.isEmpty => toApply.headOption.foreach {
      case header: Header =>
        val newState: UtxoState = state.copy(height = Height @@ header.height, lastBlockTimestamp = header.timestamp)
        val newHistory: History = history.reportModifierIsValid(header)
        context.system.eventStream.publish(SemanticallySuccessfulModifier(header))
        val newToApply: List[PersistentModifier] = toApply.drop(1)
        if (newToApply.nonEmpty) {
          logger.info(s"Header ${header.encodedId} with height ${header.height} in receive modifierApplication applied successfully." +
            s"Starting new modifier application.")
          self ! StartModifiersApplying
          context.become(modifierApplication(
            newToApply,
            UpdateInformation(newHistory, newState, none, none, updateInformation.suffix :+ header)
          ))
        } else {
          logger.info(s"Finished modifiers application. Become to modifiersApplicationCompleted.")
          self ! ModifiersApplicationFinished
          context.become(modifiersApplicationCompleted(
            UpdateInformation(newHistory, newState, none, none, updateInformation.suffix :+ header)
          ))
        }
      case block: Block =>
        logger.info(s"Start block ${block.encodedId} validation to the state")
        val lastTxId: ModifierId = block.payload.txs.last.id
        val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
        val height: Height = Height @@ block.header.height
        block.payload.txs.foreach {
          case tx if tx.id sameElements lastTxId =>
            val txValidator: ActorRef = context.actorOf(
              TransactionsValidator.props(state, 0L, tx, height),
              s"validatorFor:${tx.encodedId}"
            )
            validatorsQueue = validatorsQueue.updated(tx.encodedId, txValidator)
          case tx =>
            val txValidator: ActorRef = context.actorOf(
              TransactionsValidator.props(state, totalFees + EncrySupplyController.supplyAt(height), tx, height),
              s"validatorFor:${tx.encodedId}"
            )
            validatorsQueue = validatorsQueue.updated(tx.encodedId, txValidator)
        }
        infoAboutCurrentFoldIteration = (
          toApply,
          updateInformation,
          block.some
        )
        context.become(awaitingTransactionsValidation)
    }
    case StartModifiersApplying => //todo redundant iterations while updateInformation.failedMod.nonEmpty
      self ! StartModifiersApplying
      context.become(modifierApplication(toApply.drop(1), updateInformation))
    case _ =>
  }

  def modifiersApplicationCompleted(ui: UpdateInformation): Receive = {
    case ModifiersApplicationFinished => ui.failedMod match {
      case Some(_) =>
        self ! StartModifiersApplicationOnStateApplicator(ui.alternativeProgressInfo.get, ui.suffix)
        context.become(updateState)
      case None =>
        logger.info(s"Send request for the next modifier to the history applicator")
        historyApplicator ! RequestNextModifier
        isInProgress = false
        context.become(updateState)
    }
  }

  def awaitingTransactionsValidation: Receive = {
    case TransactionValidatedSuccessfully(tx) =>
      validatorsQueue = validatorsQueue - tx.encodedId //overhead
      validatedTxs = tx :: validatedTxs //overhead
      if (validatorsQueue.isEmpty) {
        val combinedStateChange = combineAll(validatedTxs.map(UtxoState.tx2StateChange))
        infoAboutCurrentFoldIteration._3.foreach { block =>
          state.storage.insert(
            StorageVersion !@@ block.id,
            combinedStateChange.outputsToDb.toList,
            combinedStateChange.inputsToDb.toList
          )
          if (infoAboutCurrentFoldIteration._1.isEmpty) {
            logger.info(s"Finished modifiers application. Become to modifiersApplicationCompleted.")
            self ! ModifiersApplicationFinished
            context.become(modifiersApplicationCompleted(
              infoAboutCurrentFoldIteration._2.copy(state = UtxoState(
                state.storage,
                Height @@ block.header.height,
                block.header.timestamp
              ))
            ))
          } else {
            self ! StartModifiersApplying
            context.become(modifierApplication(infoAboutCurrentFoldIteration._1.toList, infoAboutCurrentFoldIteration._2))
          }
        }
      }
    case TransactionValidatedFailure(_, _) =>
      context.children.foreach(_ ! Kill) //P_Pill
      infoAboutCurrentFoldIteration._3.foreach { block =>
        val (newHis: History, newProgressInfo: ProgressInfo) =
          history.reportModifierIsInvalid(block)
        context.system.eventStream.publish(SemanticallyFailedModification(block, List(StateModifierApplyError("1234"))))
        self ! StartModifiersApplying
        context.become(modifierApplication(
          infoAboutCurrentFoldIteration._1.toList,
          UpdateInformation(newHis, state, Some(block), Some(newProgressInfo), infoAboutCurrentFoldIteration._2.suffix)
        ))
      }

  }

  def updateState: Receive = {
    case NewModifierNotification if !isInProgress =>
      logger.info(s"StateApplicator got notification about new modifier. Send request for new one.")
      sender() ! RequestNextModifier

    case NewModifierNotification =>
      logger.info(s"StateApplicator got notification about new modifier but inProgress is $isInProgress.")

    case StartModifiersApplicationOnStateApplicator(progressInfo, suffixApplied) =>
      isInProgress = true
      logger.info(s"Starting applying to the state.")
      val branchPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
      val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]) =
        if (progressInfo.chainSwitchingNeeded) branchPointOpt
          .map(branchPoint =>
            if (!state.version.sameElements(branchPoint))
              state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied.toIndexedSeq, ModifierId !@@ branchPoint)
            else Success(state) -> IndexedSeq.empty[PersistentModifier]
          ).getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
        else Success(state) -> suffixApplied
      stateToApplyTry match {
        case Failure(exception) =>
          context.system.eventStream.publish(RollbackFailed(branchPointOpt))
          EncryApp.forceStopApplication(500, s"Rollback failed: $exception")
        case Success(stateToApply) =>
          logger.info(s"Successfully applied to the state. Starting modifiers applying.")
          context.system.eventStream.publish(RollbackSucceed(branchPointOpt))
          self ! StartModifiersApplying
          context.become(modifierApplication(
            progressInfo.toApply.toList,
            UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
          ))
      }
  }

  override def receive: Receive = updateState

  def trimChainSuffix(suffix: IndexedSeq[PersistentModifier], rollbackPoint: ModifierId):
  IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }
}

object StateApplicator {

  case object NotificationAboutSuccessfullyAppliedModifier

  case object StartModifiersApplying

  case object ModifiersApplicationFinished

  case object ValidationFinished

  final case class StartApplyingModifiersForState(modifiersToApply: Seq[PersistentModifier],
                                                  accumulatedUpdateInformation: UpdateInformation)


  final case class ModifierForStateAppliedSuccessfully(accumulatedUpdateInformation: UpdateInformation,
                                                       lastsToApply: Seq[PersistentModifier])

  case object RequestNextModifier

  case object ModifierValidatedSuccessfully

  def props(setting: EncryAppSettings, history: History, historyApplicator: ActorRef,state: UtxoState): Props =
    Props(new StateApplicator(setting, history, historyApplicator, state))
}