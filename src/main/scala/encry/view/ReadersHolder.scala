package encry.view

import akka.actor.{Actor, ActorRef, Props}
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedState, NodeViewChange}
import encry.view.NodeViewHolder.ReceivableMessages.GetNodeViewChanges
import encry.view.ReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.state.UtxoStateReader

class ReadersHolder(nvhRef: ActorRef) extends Actor {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    nvhRef ! GetNodeViewChanges(history = true, state = true, vault = true)
  }

  var historyReaderOpt: Option[EncryHistoryReader] = None
  var stateReaderOpt: Option[UtxoStateReader] = None

  override def receive: Receive = {
    case ChangedHistory(reader: EncryHistoryReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
      historyReaderOpt = Some(reader)
    case ChangedState(reader: UtxoStateReader@unchecked) if reader.isInstanceOf[UtxoStateReader] =>
      stateReaderOpt = Some(reader)
    case GetReaders => sender ! Readers(historyReaderOpt, stateReaderOpt)
    case GetDataFromHistory => historyReaderOpt.foreach(sender ! _)
    case _ =>
  }
}

object ReadersHolder {

  def props(nvhRef: ActorRef): Props = Props(new ReadersHolder(nvhRef))

  case object GetDataFromHistory

  case object GetReaders

  case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader])

}