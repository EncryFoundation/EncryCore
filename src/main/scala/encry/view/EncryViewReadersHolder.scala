package encry.view

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import encry.view.EncryViewReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.mempool.EncryMempoolReader
import encry.view.state.UtxoStateReader
import scorex.core.NodeViewHolder.ReceivableMessages.GetNodeViewChanges
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool, ChangedState, NodeViewChange}
import scorex.core.utils.ScorexLogging

class EncryViewReadersHolder(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    viewHolderRef ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
  }

  var historyReaderOpt: Option[EncryHistoryReader] = None
  var stateReaderOpt: Option[UtxoStateReader] = None
  var mempoolReaderOpt: Option[EncryMempoolReader] = None

  override def receive: Receive = {
    case ChangedHistory(reader: EncryHistoryReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
      historyReaderOpt = Some(reader)

    case ChangedState(reader: UtxoStateReader@unchecked) if reader.isInstanceOf[UtxoStateReader] =>
      stateReaderOpt = Some(reader)

    case ChangedMempool(reader: EncryMempoolReader@unchecked) if reader.isInstanceOf[EncryMempoolReader] =>
      mempoolReaderOpt = Some(reader)

    case GetReaders =>
      sender ! Readers(historyReaderOpt, stateReaderOpt, mempoolReaderOpt)

    case GetDataFromHistory(f) =>
      historyReaderOpt match {
        case Some(historyReader) =>
          sender() ! f(historyReader)
        case None =>
          log.warn("Trying to get data from undefined history reader")
      }

    case _ => // Do nothing.
  }
}

object EncryViewReadersHolder {

  case class GetDataFromHistory[A](f: EncryHistoryReader => A)

  case object GetReaders

  case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader], m: Option[EncryMempoolReader])
}

object EncryReadersHolderRef {

  def props(viewHolderRef: ActorRef): Props = Props(new EncryViewReadersHolder(viewHolderRef))

  def apply(viewHolderRef: ActorRef)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolderRef))

  def apply(viewHolderRef: ActorRef,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolderRef), name)
}
