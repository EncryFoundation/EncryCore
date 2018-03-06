package encry.local.scanner

import akka.actor.{Actor, ActorRef}
import encry.local.scanner.storage.IndexStorage
import encry.settings.EncryAppSettings
import io.iohk.iodb.Store
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.Subscribe
import scorex.core.utils.ScorexLogging

class EncryScanner(settings: EncryAppSettings,
                   viewHolderRef: ActorRef,
                   indexStore: Store) extends Actor with ScorexLogging {

  protected lazy val storage: IndexStorage = new IndexStorage(indexStore)

  override def preStart(): Unit = {
    val events = Seq(NodeViewHolder.EventType.StateChanged)
    viewHolderRef ! Subscribe(events)
  }

  override def receive: Receive = ???
}
