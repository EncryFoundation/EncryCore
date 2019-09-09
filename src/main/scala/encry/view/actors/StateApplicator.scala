package encry.view.actors

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.consensus.EncrySupplyController
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.NodeViewSynchronizer.ReceivableMessages.{RollbackFailed, RollbackSucceed, SemanticallyFailedModification, SemanticallySuccessfulModifier}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.TransactionsInBlock
import encry.utils.CoreTaggedTypes
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewHolder.UpdateInformation
import encry.view.actors.HistoryApplicator.ModifierForStateApplicator
import encry.view.actors.StateApplicator.{IterateNextModifier, ModifierForStateAppliedSuccessfully, ModifierValidatedSuccessfully, StartTransactionsValidationForState}
import encry.view.actors.TransactionsValidator.{TransactionValidatedFailure, TransactionValidatedSuccessfully}
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId}
import supertagged.@@

import scala.collection.IndexedSeq
import scala.collection.immutable.HashMap
import scala.util.{Failure, Success, Try}

class StateApplicator(setting: EncryAppSettings, history: History) extends Actor with StrictLogging {
  var state: UtxoState = UtxoState.create(UtxoState.getStateDir(setting), None, setting, None)

  var validatorsQueue: HashMap[String, ActorRef] = HashMap.empty[String, ActorRef]

  var toApplyUi: (Seq[PersistentModifier], UpdateInformation) =
    (Seq.empty, UpdateInformation(history, state, None, None, IndexedSeq.empty))

  //todo add supervisor strategy

  override def receive: Receive = {
    case ModifierForStateApplicator(progressInfo, suffixApplied) =>
      //separate here for headers and blocks validation

      //create N validators for each transaction in modifier

      //await finishing all validators

      //sent request to HistoryApplicator for next modifier

      val branchPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
      val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]) =
        if (progressInfo.chainSwitchingNeeded) branchPointOpt.map(branchPoint =>
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
          context.system.eventStream.publish(RollbackSucceed(branchPointOpt))
          val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
          self ! StartTransactionsValidationForState(progressInfo.toApply, u0)
      }

    case StartTransactionsValidationForState(toApply, ui) if toApply.nonEmpty =>
      if (ui.failedMod.isEmpty) toApply.headOption.foreach {
        case block: Block =>
          val lastTxId = block.payload.txs.last.id
          val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
          block.payload.txs.foreach { tx =>
            if (tx.id sameElements lastTxId) {
              val txValidator: ActorRef = context.actorOf(
                TransactionsValidator.props(state, 0L, tx, Height @@ block.header.height),
                s"validatorFor:${tx.encodedId}"
              )
              validatorsQueue = validatorsQueue.updated(tx.encodedId, txValidator)
            } else {
              val txValidator: ActorRef = context.actorOf(
                TransactionsValidator
                  .props(state, totalFees + EncrySupplyController.supplyAt(Height @@ block.header.height), tx, Height @@ block.header.height),
                s"validatorFor:${tx.encodedId}"
              )
              validatorsQueue = validatorsQueue.updated(tx.encodedId, txValidator)
            }
          }
          toApplyUi = (toApply, ui)
        case _ => //todo add later
      }

    case TransactionValidatedSuccessfully(tx) =>
      validatorsQueue = validatorsQueue - tx.encodedId
      if (validatorsQueue.isEmpty) self ! IterateNextModifier

    case IterateNextModifier =>

    case TransactionValidatedFailure(tx, ex) =>

    case ModifierValidatedSuccessfully =>
    //historyRef ! RequestNextModifier

    case ModifierForStateAppliedSuccessfully(ui, lastsToApply) if lastsToApply.nonEmpty =>
      //check for empty
      lastsToApply.headOption.foreach { mod =>
        state.applyModifier(mod) match {
          case Left(value) =>
            val j = history.reportModifierIsInvalid(mod)
            context.system.eventStream.publish(SemanticallyFailedModification(mod, value))
            self ! ModifierForStateAppliedSuccessfully(
              UpdateInformation(j._1, ui.state, Some(mod), Some(j._2), ui.suffix),
              lastsToApply.drop(1)
            )
          case Right(value) =>
            val newHistory = history.reportModifierIsValid(mod)
            context.system.eventStream.publish(SemanticallySuccessfulModifier(mod))
            self ! ModifierForStateAppliedSuccessfully(
              UpdateInformation(newHistory, value, None, None, ui.suffix :+ mod),
              lastsToApply.drop(1)
            )
        }
      }
    case ModifierForStateAppliedSuccessfully(ui, _) =>
      ui.failedMod match {
        case Some(_) => //self ! //message
        case None => //history ! requestnextmod
      }

    case _ =>
  }

  def applyToState(progressInfo: ProgressInfo,
                   suffixApplied: Seq[PersistentModifier]): Unit = {
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]@unchecked) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint =>
          if (!state.version.sameElements(branchPoint))
            state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied.toIndexedSeq, ModifierId !@@ branchPoint)
          else Success(state) -> IndexedSeq()
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
      } else Success(state) -> suffixApplied
    stateToApplyTry match {
      case Failure(exception) =>
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500, s"Rollback failed: $exception")
      case Success(value) =>
        val u0: UpdateInformation = UpdateInformation(history, value, None, None, suffixTrimmed)
        progressInfo.toApply.headOption.foreach { firstMod =>
          value.applyModifier(firstMod) match {
            case Left(v) =>
              val j = history.reportModifierIsInvalid(firstMod)
              context.system.eventStream.publish(SemanticallyFailedModification(firstMod, v))
              self ! ModifierForStateAppliedSuccessfully(
                UpdateInformation(j._1, u0.state, Some(firstMod), Some(j._2), u0.suffix),
                progressInfo.toApply.drop(1)
              )
            case Right(state1) =>
              val newHistory = history.reportModifierIsValid(firstMod)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(firstMod))
              self ! ModifierForStateAppliedSuccessfully(
                UpdateInformation(newHistory, state1, None, None, u0.suffix :+ firstMod),
                progressInfo.toApply.drop(1)
              )
          }
        }
    }
  }

  def trimChainSuffix(suffix: IndexedSeq[PersistentModifier], rollbackPoint: ModifierId):
  IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }
}

object StateApplicator {

  case object IterateNextModifier

  final case class StartTransactionsValidationForState(modifiersToApply: Seq[PersistentModifier],
                                                       accumulatedUpdateInformation: UpdateInformation)


  final case class ModifierForStateAppliedSuccessfully(accumulatedUpdateInformation: UpdateInformation,
                                                       lastsToApply: Seq[PersistentModifier])

  case object RequestNextModifier

  case object ModifierValidatedSuccessfully

}