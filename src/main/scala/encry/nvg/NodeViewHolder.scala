package encry.nvg

import akka.actor.{ Actor, Props }

class NodeViewHolder extends Actor {
  override def receive: Receive = ???
}

object NodeViewHolder {
  def props: Props = Props(new NodeViewHolder)
}
