package encry.network

import java.net.{InetAddress, InetSocketAddress}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{HistoryComparisonResult, Older, Unknown}
import encry.network.ConnectedPeersList.{LastUptime, PeerInfo}
import encry.network.PeerConnectionHandler.{ConnectedPeer, ConnectionType, Outgoing}
import encry.network.PrioritiesCalculator.PeersPriorityStatus.{InitialPriority, PeersPriorityStatus}
import encry.settings.EncryAppSettings

final class ConnectedPeersList(settings: EncryAppSettings) extends StrictLogging {

  private var peers: Map[InetSocketAddress, PeerInfo] = Map.empty

  def contains(peer: InetSocketAddress): Boolean = peers.contains(peer)

  def size: Int = peers.size

  def initializePeer(peer: ConnectedPeer): Unit = {
    val peerInfo: PeerInfo = PeerInfo(
      Unknown,
      InitialPriority(),
      peer,
      Outgoing,
      LastUptime(0)
    )
    peers = peers.updated(peer.socketAddress, peerInfo)
  }

  def updatePeerInfo(peer: InetSocketAddress, peerInfo: PeerInfo): Unit = peers = peers.updated(peer, peerInfo)

  def removePeer(peer: InetSocketAddress): Unit = peers -= peer

  def updatePeersPriorityStatus(statistic: Map[InetSocketAddress, PeersPriorityStatus]): Unit = statistic.foreach {
    case (peer, status) => peers.get(peer) match {
      case Some(peerInfo) =>
        logger.info(s"Updating priority status for peer $peer... $status.")
        peers = peers.updated(peer, peerInfo.copy(peerPriorityStatus = status))
      case None => logger.info(s"Can't update peer's $peer priority. No such peer in connected peers list.")
    }
  }

  def updatePeerComparisonStatus(peer: ConnectedPeer, status: HistoryComparisonResult): Unit =
    peers.get(peer.socketAddress) match {
      case Some(value) =>
        val newPeerInfo: PeerInfo = value.copy(historyComparisonResult = status)
        peers = peers.updated(peer.socketAddress, newPeerInfo)
      case None =>
        //todo can we have such case??
        logger.info(s"Trying to update history comparison result but there is no such peer in connected collection.")
    }

  def containsOlderPeer: Boolean = peers.exists(p => p._2.historyComparisonResult == Older)

  def updateUptime(peer: InetSocketAddress): Unit = peers.get(peer) match {
    case Some(info) =>
      val newInfo: PeerInfo = info.copy(lastUptime = LastUptime(System.currentTimeMillis()))
      peers = peers.updated(peer, newInfo)
    case None => //todo do we have such case?
  }

  def getPeersF[T](p: (InetSocketAddress, PeerInfo) => Boolean,
                   f: (InetSocketAddress, PeerInfo) => T): Iterable[T] =
    peers.filter(k => p(k._1, k._2)).map(x => f(x._1, x._2))

}

object ConnectedPeersList {

  final case class LastUptime(time: Long) extends AnyVal

  final case class PeerInfo(historyComparisonResult: HistoryComparisonResult,
                            peerPriorityStatus: PeersPriorityStatus,
                            connectedPeer: ConnectedPeer,
                            connectionType: ConnectionType,
                            lastUptime: LastUptime)

}