package encry.nvg

import akka.actor.{Actor, ActorRef, Props}
import cats.syntax.option.none
import encry.network.NetworkRouter.ModifierFromNetwork
import encry.nvg.IntermediaryNVHView.IntermediaryNVHViewActions.{RegisterHistory, RegisterState}
import encry.nvg.NodeViewHolder.NodeView
import encry.view.history.History
import encry.view.state.UtxoState

class IntermediaryNVHView() extends Actor {

  override def receive: Receive = ???

  def awaitingViewActors(history: Option[ActorRef], state: Option[ActorRef]): Receive = {
    case RegisterHistory if state.isEmpty =>
      context.become(awaitingViewActors(Some(sender()), state), discardOld = true)
    case RegisterHistory =>
      context.become(viewReceive(sender(), state.get))
    case RegisterState if history.isEmpty =>
      context.become(awaitingViewActors(history, Some(sender())), discardOld = true)
    case RegisterHistory =>
      context.become(viewReceive(history.get, sender()))
  }

  def viewReceive(history: ActorRef, state: ActorRef): Receive = {
    case ModifierFromNetwork(remote, typeId, modifierId, modifierBytes) => history ! ModifierFromNetwork
  }

  def restoreState(influxRef: Option[ActorRef] = none): Unit = {

  }
}

object IntermediaryNVHView {

  sealed trait IntermediaryNVHViewActions
  object IntermediaryNVHViewActions {
    case object RegisterHistory extends IntermediaryNVHViewActions
    case object RegisterState extends IntermediaryNVHViewActions
  }

  def props(): Props = Props(new IntermediaryNVHView())
}
