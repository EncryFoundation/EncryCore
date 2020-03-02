package encry.nvg

import akka.actor.{Actor, Props}
import encry.view.history.{History, HistoryReader}

class NodeViewHolder extends Actor {
  override def receive: Receive = ???
}

object NodeViewHolder {

  final case class UpdateHistoryReader(history: HistoryReader) extends AnyVal

  def props: Props = Props(new NodeViewHolder)
}
