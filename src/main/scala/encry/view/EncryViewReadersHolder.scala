package encry.view

import akka.actor.Actor
import encry.view.EncryViewReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.mempool.EncryMempoolReader
import encry.view.state.UtxoStateReader
import encry.EncryApp._
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool, ChangedState, NodeViewChange}
import encry.utils.EncryLogging

class EncryViewReadersHolder extends Actor with EncryLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
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
    case GetReaders => sender ! Readers(historyReaderOpt, stateReaderOpt, mempoolReaderOpt)
    case GetDataFromHistory(f) => historyReaderOpt.foreach(sender ! f(_))
    case _ =>
  }
}

object EncryViewReadersHolder {

  case class GetDataFromHistory[A](f: EncryHistoryReader => A)

  case object GetReaders

  case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader], m: Option[EncryMempoolReader])

}