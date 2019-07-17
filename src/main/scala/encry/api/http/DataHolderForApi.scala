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
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None),
                   blockInfo: BlockInfo = BlockInfo(0, "", 0, ""),
                   allPeers: Seq[InetSocketAddress] = Seq.empty): Receive = {
    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, qty, minerStatus, blockInfo, allPeers))

    case ChangedHistory(reader: EncryHistoryReader) =>
    val info: BlockInfo = BlockInfo(reader.bestHeaderOpt.map(_.height).getOrElse(0), reader.bestHeaderOpt.map(_.encodedId).getOrElse(""),
      reader.bestBlockOpt.map(_.header.height).getOrElse(0) ,reader.bestBlockOpt.map(_.header.encodedId).getOrElse(""))
      context.become(workingCycle(blackList, connectedPeers, Some(reader), state, transactionsOnMinerActor, minerStatus,
        info, allPeers))

    case ChangedState(reader: UtxoStateReader)  =>
      context.become(workingCycle(blackList, connectedPeers, history, Some(reader), transactionsOnMinerActor, minerStatus,
        blockInfo, allPeers))

    case UpdatingMinerStatus(status) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, transactionsOnMinerActor, status,
        blockInfo, allPeers))

    case UpdatingConnectedPeers(peers) =>
      context.become(workingCycle(blackList, peers, history, state, transactionsOnMinerActor, minerStatus,
        blockInfo, allPeers))

    case UpdatingAllKnownPeers(peers) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, transactionsOnMinerActor, minerStatus,
        blockInfo, peers))

    case GetConnectedPeers      => sender() ! connectedPeers
    case GetDataFromHistory     => history.foreach(sender() ! _)
    case GetMinerStatus         => sender() ! minerStatus
    case GetReaders             => sender() ! Readers(history, state)
    case GetTransactionsNumber  => sender() ! transactionsOnMinerActor
    case GetBlockInfo           => sender() ! blockInfo
    case GetAllPeers            => sender() ! allPeers
    case _                      =>
  }
}

object DataHolderForApi {

  final case class UpdatingBlackListForApi(blackList: Seq[InetAddress])

  final case class UpdatingConnectedListForApi(connectedPeers: Seq[InetSocketAddress])

  final case class UpdatingTransactionsNumberForApi(number: Int) extends AnyVal

  final case class UpdatingMinerStatus(minerStatus: MinerStatus)

  final case class UpdatingConnectedPeers(peers: Seq[ConnectedPeer])

  final case class UpdatingAllKnownPeers(peers: Seq[InetSocketAddress])

  final case class BlockInfo(headerHeight: Int, headerId: String, fullHeight: Int, bestFullHeaderId: String)

  case object GetTransactionsNumber

  case object GetReaders

  case object GetConnectedPeers

  case object GetDataFromHistory

  case object GetMinerStatus

  case object GetAllPeers

  case object GetBlockInfo

  final case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader])

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}