package encry.view

import akka.actor.{Actor, ActorRef}
import encry.view.EncryViewReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.mempool.EncryMempoolReader
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder._
import scorex.core.transaction.state.StateReader
import scorex.core.utils.ScorexLogging

class EncryViewReadersHolder(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    val vhEvents = Seq(
      NodeViewHolder.EventType.HistoryChanged,
      NodeViewHolder.EventType.StateChanged,
      NodeViewHolder.EventType.MempoolChanged,
      NodeViewHolder.EventType.VaultChanged,
    )
    viewHolderRef ! Subscribe(vhEvents)
    viewHolderRef ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
  }

  var historyReaderOpt: Option[EncryHistoryReader] = None
  var stateReaderOpt: Option[StateReader] = None
  var mempoolReaderOpt: Option[EncryMempoolReader] = None

  override def receive: Receive = {
    case ChangedHistory(reader: EncryHistoryReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
      historyReaderOpt = Some(reader)

    case ChangedState(reader: StateReader@unchecked) if reader.isInstanceOf[StateReader] =>
      stateReaderOpt = Some(reader)

    case ChangedMempool(reader: EncryMempoolReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
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

  case class Readers(h: Option[EncryHistoryReader], s: Option[StateReader], m: Option[EncryMempoolReader])
}
