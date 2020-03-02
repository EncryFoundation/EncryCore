package encry.nvg

import akka.actor.{ Actor, Props }

class NetworkMessagesProcessor extends Actor {
  override def receive: Receive = ???
}

object NetworkMessagesProcessor {
  def props: Props = Props(new NetworkMessagesProcessor)
}
