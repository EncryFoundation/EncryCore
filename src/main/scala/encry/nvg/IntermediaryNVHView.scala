package encry.nvg

import akka.actor.{Actor, ActorRef, Props}
import encry.network.NetworkRouter.ModifierFromNetwork
import encry.view.history.History
import encry.view.state.UtxoState

class IntermediaryNVHView() extends Actor {

  override def receive: Receive = ???

  def viewReceive(history: ActorRef, state: ActorRef): Receive = {
    case ModifierFromNetwork(remote, typeId, modifierId, modifierBytes) => history ! ModifierFromNetwork
  }
}

object IntermediaryNVHView {
  def props(): Props = Props(new IntermediaryNVHView())
}
