package encry.network

import java.net.{InetAddress, InetSocketAddress}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{HistoryComparisonResult, Older, Unknown}
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
      LastUptime()
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
      case None => //todo can we have such case??
    }

  def getPeersForDeliveryManager: Map[InetAddress, (ConnectedPeer, HistoryComparisonResult, PeersPriorityStatus)] =
    peers.map(x => x._1 -> (x._2.connectedPeer, x._2.historyComparisonResult, x._2.peerPriorityStatus))

  def getPeersWithoutYounger: Map[ConnectedPeer, HistoryComparisonResult] =
    peers.map(x => x._2.connectedPeer -> x._2.historyComparisonResult)

  def containsOlderPeer: Boolean = peers.exists(p => p._2.historyComparisonResult == Older)

  def getPeersForSyncInfo: Seq[ConnectedPeer] = peers
    .filter(p =>
      p._2.lastUptime.time + settings.network.syncInterval._1 > System.currentTimeMillis() &&
        (p._2.historyComparisonResult == Older || p._2.historyComparisonResult == Unknown))
    .map { p =>
      updateUptime(p._1)
      p._2.connectedPeer
    }.toSeq

  def updateUptime(peer: InetAddress): Unit = peers.get(peer) match {
    case Some(info) =>
      val newInfo: PeerInfo = info.copy(lastUptime = LastUptime())
      peers = peers.updated(peer, newInfo)
    case None => //todo do we have such case?
  }

  def getPeers: Map[InetAddress, PeerInfo] = peers

}

object ConnectedPeersList {

  final case class LastUptime(time: Long = System.currentTimeMillis()) extends AnyVal

  final case class PeerInfo(historyComparisonResult: HistoryComparisonResult,
                            peerPriorityStatus: PeersPriorityStatus,
                            connectedPeer: ConnectedPeer,
                            connectionType: ConnectionType,
                            lastUptime: LastUptime)

}