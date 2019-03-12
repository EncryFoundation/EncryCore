package encry.view

import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import encry.Starter.NVHActorIsReady
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool, ChangedState, NodeViewChange}
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import encry.view.ReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import encry.view.history.EncryHistoryReader
import encry.view.mempool.MempoolReader
import encry.view.state.UtxoStateReader

class ReadersHolder extends Actor with StrictLogging {

  override def preStart(): Unit = context.system.eventStream.subscribe(self, classOf[NodeViewChange])

  var historyReaderOpt: Option[EncryHistoryReader] = None
  var stateReaderOpt: Option[UtxoStateReader] = None
  var mempoolReaderOpt: Option[MempoolReader] = None

  override def receive: Receive = {
    case NVHActorIsReady =>
      logger.info(s"Got NVHActorIsReady from $sender. Sending request for nodeViewChanges.")
      sender() ! GetNodeViewChanges(history = true, state = true, vault = true, mempool = true)
    case ChangedHistory(reader: EncryHistoryReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
      historyReaderOpt = Some(reader)
    case ChangedState(reader: UtxoStateReader@unchecked) if reader.isInstanceOf[UtxoStateReader] =>
      stateReaderOpt = Some(reader)
    case ChangedMempool(reader: MempoolReader@unchecked) if reader.isInstanceOf[MempoolReader] =>
      mempoolReaderOpt = Some(reader)
    case GetReaders => sender ! Readers(historyReaderOpt, stateReaderOpt, mempoolReaderOpt)
    case GetDataFromHistory => historyReaderOpt.foreach(sender ! _)
    case _ =>
  }
}

object ReadersHolder {
  case object GetDataFromHistory
  case object GetReaders
  case class  Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader], m: Option[MempoolReader])
}