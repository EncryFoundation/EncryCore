package encry.api.http

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi._
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedState, NodeViewChange}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.EncryHistoryReader
import encry.view.state.UtxoStateReader
import encry.local.miner.Miner.MinerStatus
import encry.network.PeerConnectionHandler.ConnectedPeer

class DataHolderForApi(settings: EncryAppSettings,
                       ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
  }

  override def receive: Receive = workingCycle()

  def workingCycle(blackList: Seq[InetAddress] = Seq.empty,
                   connectedPeers: Seq[ConnectedPeer] = Seq.empty,
                   history: Option[EncryHistoryReader] = None,
                   state: Option[UtxoStateReader] = None,
                   transactionsOnMinerActor: Int = 0,
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None)): Receive = {
    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, qty, minerStatus))

    case ChangedHistory(reader: EncryHistoryReader@unchecked) if reader.isInstanceOf[EncryHistoryReader] =>
      context.become(workingCycle(blackList, connectedPeers, Some(reader), state, transactionsOnMinerActor, minerStatus))

    case ChangedState(reader: UtxoStateReader@unchecked) if reader.isInstanceOf[UtxoStateReader] =>
      context.become(workingCycle(blackList, connectedPeers, history, Some(reader), transactionsOnMinerActor, minerStatus))

    case UpdatingMinerStatus(status) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, transactionsOnMinerActor, status))

    case UpdatingConnectedPeers(peers) =>
      context.become(workingCycle(blackList, peers, history, state, transactionsOnMinerActor, minerStatus))

    case GetConnectedPeers => sender() ! connectedPeers
    case GetDataFromHistory => history.foreach(sender() ! _)
    case GetMinerStatus => sender() ! minerStatus
    case GetReaders => sender() ! Readers(history, state)
    case GetTransactionsNumber => sender() ! transactionsOnMinerActor
    case _ =>
  }
}

object DataHolderForApi {

  final case class UpdatingBlackListForApi(blackList: Seq[InetAddress])

  final case class UpdatingConnectedListForApi(connectedPeers: Seq[InetSocketAddress])

  final case class UpdatingTransactionsNumberForApi(number: Int) extends AnyVal

  final case class UpdatingMinerStatus(minerStatus: MinerStatus)

  final case class UpdatingConnectedPeers(peers: Seq[ConnectedPeer])

  case object GetTransactionsNumber

  case object GetReaders

  case object GetConnectedPeers

  case object GetDataFromHistory

  case object GetMinerStatus

  final case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader])

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}