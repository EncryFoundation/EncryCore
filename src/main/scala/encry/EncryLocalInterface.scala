package encry

import akka.actor.{Actor, ActorRef}
import encry.modifiers.mempool.EncryBaseTransaction
import encry.view.EncryBaseNodeViewHolder
import encry.view.EncryBaseNodeViewHolder._
import scorex.core.{ModifierId, NodeViewHolder, PersistentNodeViewModifier}
import scorex.core.utils.ScorexLogging

trait EncryLocalInterface[TX <: EncryBaseTransaction, PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  import EncryLocalInterface._

  val viewHolderRef: ActorRef

  override def preStart(): Unit = {
    val events = Seq(
      EncryBaseNodeViewHolder.EventType.SuccessfulTransaction,
      EncryBaseNodeViewHolder.EventType.FailedTransaction,

      EncryBaseNodeViewHolder.EventType.StartingPersistentModifierApplication,
      EncryBaseNodeViewHolder.EventType.SyntacticallyFailedPersistentModifier,
      EncryBaseNodeViewHolder.EventType.SemanticallyFailedPersistentModifier,
      EncryBaseNodeViewHolder.EventType.SuccessfulSyntacticallyValidModifier,
      EncryBaseNodeViewHolder.EventType.SuccessfulSemanticallyValidModifier,

      EncryBaseNodeViewHolder.EventType.OpenSurfaceChanged,
      EncryBaseNodeViewHolder.EventType.StateChanged,
      EncryBaseNodeViewHolder.EventType.FailedRollback
    )
    viewHolderRef ! Subscribe(events)
  }

  private def viewHolderEvents: Receive = {
    case st: SuccessfulTransaction[TX] =>
      onSuccessfulTransaction(st.transaction)

    case ft: FailedTransaction[TX] =>
      onFailedTransaction(ft.transaction)


    case stm: StartingPersistentModifierApplication[PMOD] =>
      onStartingPersistentModifierApplication(stm.modifier)

    case syns: SyntacticallySuccessfulModifier[PMOD] =>
      onSyntacticallySuccessfulModification(syns.modifier)

    case synf: SyntacticallyFailedModification[PMOD] =>
      onSyntacticallyFailedModification(synf.modifier)

    case sems: SemanticallySuccessfulModifier[PMOD] =>
      onSemanticallySuccessfulModification(sems.modifier)

    case semf: SemanticallyFailedModification[PMOD] =>
      onSemanticallyFailedModification(semf.modifier)

    case surf: NewOpenSurface =>
      onNewSurface(surf.newSurface)

    case RollbackFailed =>
      onRollbackFailed()
  }


  protected def onSuccessfulTransaction(tx: TX): Unit
  protected def onFailedTransaction(tx: TX): Unit


  protected def onStartingPersistentModifierApplication(pmod: PMOD): Unit

  protected def onSyntacticallySuccessfulModification(mod: PMOD): Unit
  protected def onSyntacticallyFailedModification(mod: PMOD): Unit

  protected def onSemanticallySuccessfulModification(mod: PMOD): Unit
  protected def onSemanticallyFailedModification(mod: PMOD): Unit

  protected def onNewSurface(newSurface: Seq[ModifierId]): Unit
  protected def onRollbackFailed(): Unit


  protected def onNoBetterNeighbour(): Unit
  protected def onBetterNeighbourAppeared(): Unit

  override def receive: Receive = viewHolderEvents orElse {
    case NoBetterNeighbour =>
      onNoBetterNeighbour()
    case BetterNeighbourAppeared =>
      onBetterNeighbourAppeared()
    case lt: LocallyGeneratedTransaction[TX] =>
      viewHolderRef ! lt
    case lm: LocallyGeneratedModifier[PMOD] =>
      viewHolderRef ! lm
    case a: Any => log.error("Strange input: " + a)
  }
}

object EncryLocalInterface {

  case object NoBetterNeighbour

  case object BetterNeighbourAppeared

  case class LocallyGeneratedTransaction[TX <: EncryBaseTransaction](tx: TX)

  case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)
}
