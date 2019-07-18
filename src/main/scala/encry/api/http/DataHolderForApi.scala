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
import encry.network.BlackList
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.PeerConnectionHandler.ConnectedPeer
import org.encryfoundation.common.modifiers.history.{Block, Header}

class DataHolderForApi(settings: EncryAppSettings,
                       ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
  }

  override def receive: Receive = workingCycle()

  def workingCycle(blackList: Seq[(InetAddress, (BanReason, BanTime, BanType))] = Seq.empty,
                   connectedPeers: Seq[ConnectedPeer] = Seq.empty,
                   history: Option[EncryHistoryReader] = None,
                   state: Option[UtxoStateReader] = None,
                   transactionsOnMinerActor: Int = 0,
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None),
                   blockInfo: BlockAndHeaderInfo = BlockAndHeaderInfo(None, None),
                   allPeers: Seq[InetSocketAddress] = Seq.empty): Receive = {
    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, qty, minerStatus, blockInfo, allPeers))

    case ChangedHistory(reader: EncryHistoryReader) =>
    val info: BlockAndHeaderInfo = BlockAndHeaderInfo(reader.bestHeaderOpt, reader.bestBlockOpt)
      context.become(workingCycle(blackList, connectedPeers, Some(reader), state, transactionsOnMinerActor, minerStatus,
        info, allPeers))

    case ChangedState(reader: UtxoStateReader)  =>
      context.become(workingCycle(blackList, connectedPeers, history, Some(reader), transactionsOnMinerActor, minerStatus,
        blockInfo, allPeers))

    case UpdatingMinerStatus(status) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, transactionsOnMinerActor, status,
        blockInfo, allPeers))

    case UpdatingPeersInfo(allP, connectedP, bannedP) =>
      def mapReason(address: InetAddress, r: BanReason, t: BanTime, bt: BanType):
      (InetAddress, (BanReason, BanTime, BanType)) = address -> (r, t, bt)
     val bannedPeers: Seq[(InetAddress, (BanReason, BanTime, BanType))] = bannedP.collect((_, _, _, _) => true, mapReason)
      context.become(workingCycle(bannedPeers, connectedP, history, state, transactionsOnMinerActor, minerStatus,
        blockInfo, allP))

    case GetConnectedPeers      => sender() ! connectedPeers
    case GetDataFromHistory     => history.foreach(sender() ! _)
    case GetMinerStatus         => sender() ! minerStatus
    case GetReaders             => sender() ! Readers(history, state)
    case GetTransactionsNumber  => sender() ! transactionsOnMinerActor
    case GetBlockInfo           => sender() ! blockInfo
    case GetAllPeers            => sender() ! allPeers
    case GetBannedPeers         => sender() ! blackList
    case GetAllInfo =>
      sender() ! (
        connectedPeers,
        history,
        minerStatus,
        Readers(history, state),
        transactionsOnMinerActor,
        blockInfo,
        allPeers
      )
    case _                      =>
  }
}

object DataHolderForApi {

  final case class UpdatingBlackListForApi(blackList: Seq[InetAddress])

  final case class UpdatingConnectedListForApi(connectedPeers: Seq[InetSocketAddress])

  final case class UpdatingTransactionsNumberForApi(number: Int) extends AnyVal

  final case class UpdatingMinerStatus(minerStatus: MinerStatus)

  final case class UpdatingPeersInfo(allPeers: Seq[InetSocketAddress], connectedPeers: Seq[ConnectedPeer], blackList: BlackList)

  final case class BlockAndHeaderInfo(header: Option[Header], block: Option[Block])

  case object GetTransactionsNumber

  case object GetReaders

  case object GetConnectedPeers

  case object GetDataFromHistory

  case object GetMinerStatus

  case object GetAllPeers

  case object GetBlockInfo

  case object GetBannedPeers

  case object GetAllInfo

  final case class Readers(h: Option[EncryHistoryReader], s: Option[UtxoStateReader])

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}