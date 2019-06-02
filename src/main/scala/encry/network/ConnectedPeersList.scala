package encry.network

import java.net.{InetAddress, InetSocketAddress}

import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{Fork, HistoryComparisonResult, Unknown}
import encry.network.ConnectedPeersList.{LastUptime, PeerInfo}
import encry.network.PeerConnectionHandler.{ConnectedPeer, ConnectionType, Outgoing}
import encry.network.PrioritiesCalculator.PeersPriorityStatus.{InitialPriority, PeersPriorityStatus}
import encry.settings.EncryAppSettings

final class ConnectedPeersList(settings: EncryAppSettings) extends StrictLogging {

  private var peers: Map[InetAddress, PeerInfo] = Map.empty

  def contains(peer: InetAddress): Boolean = peers.contains(peer)

  def size: Int = peers.size

  def initializePeer(peer: ConnectedPeer): Unit = {
    val peerInfo: PeerInfo = PeerInfo(
      Unknown,
      InitialPriority(),
      peer,
      Outgoing,
      LastUptime(System.currentTimeMillis())
    )
    peers = peers.updated(peer.socketAddress.getAddress, peerInfo)
  }

  def updatePeerInfo(peer: InetAddress, peerInfo: PeerInfo): Unit = peers = peers.updated(peer, peerInfo)

  def removePeer(peer: InetAddress): Unit = peers -= peer

  def getAll: Seq[InetSocketAddress] = peers.values.map(_.connectedPeer.socketAddress).toSeq

  def getAllConnectedPeers: Seq[ConnectedPeer] = peers.values.map(_.connectedPeer).toSeq

  def updatePeersPriorityStatus(statistic: Map[InetAddress, PeersPriorityStatus]): Unit = statistic.foreach {
    case (peer, status) => peers.get(peer) match {
      case Some(peerInfo) =>
        logger.info(s"Updating priority status for peer $peer... $status.")
        peers = peers.updated(peer, peerInfo.copy(peerPriorityStatus = status))
      case None => logger.info(s"Can't update peer's $peer priority. No such peer in connected peers list.")
    }
  }

  def updatePeerComparisonStatus(peer: ConnectedPeer, status: HistoryComparisonResult): Unit =
    peers.get(peer.socketAddress.getAddress) match {
      case Some(value) =>
        val newPeerInfo: PeerInfo = value.copy(historyComparisonResult = status)
        peers = peers.updated(peer.socketAddress.getAddress, newPeerInfo)
      case None => //todo can we have such behaviour??
    }

  def getPeersForDeliveryManager: IndexedSeq[(ConnectedPeer, PeersPriorityStatus)] =
    peers.collect { case (_, info) if info.historyComparisonResult != Fork =>
      info.connectedPeer -> info.peerPriorityStatus
    }.toIndexedSeq

  def getPeersWithoutYounger: Map[ConnectedPeer, HistoryComparisonResult] =
    peers.map(x => x._2.connectedPeer -> x._2.historyComparisonResult)
}

object ConnectedPeersList {

  final case class LastUptime(time: Long) extends AnyVal

  final case class PeerInfo(historyComparisonResult: HistoryComparisonResult,
                            peerPriorityStatus: PeersPriorityStatus,
                            connectedPeer: ConnectedPeer,
                            connectionType: ConnectionType,
                            lastUptime: LastUptime)


}