package encry.local

import akka.actor.{Actor, ActorRef}
import encry.settings.EncryAppSettings
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.Subscribe
import scorex.core.utils.ScorexLogging

class StateScanner(settings: EncryAppSettings,
                   viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    val events = Seq(NodeViewHolder.EventType.StateChanged)
    viewHolderRef ! Subscribe(events)
  }

  override def receive: Receive = ???
}
