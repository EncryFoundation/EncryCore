package encry.network

import java.net.InetSocketAddress
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{HistoryComparisonResult, Unknown}
import encry.network.ConnectedPeersCollection.{LastUptime, PeerInfo}
import encry.network.PeerConnectionHandler.{ConnectedPeer, ConnectionType, Outgoing}
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus.InitialPriority
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus

final case class ConnectedPeersCollection(private val peers: Map[InetSocketAddress, PeerInfo]) extends StrictLogging {

  val size: Int = peers.size

  def contains(peer: InetSocketAddress): Boolean = peers.contains(peer)

  def initializePeer(cp: ConnectedPeer): ConnectedPeersCollection = ConnectedPeersCollection(peers.updated(
    cp.socketAddress, PeerInfo(Unknown, InitialPriority, cp, Outgoing, LastUptime(0))
  ))

  def updatePriorityStatus(stats: Map[InetSocketAddress, PeersPriorityStatus]): ConnectedPeersCollection =
    ConnectedPeersCollection(stats.foldLeft(peers) { case (oldPeers, (address, status)) =>
      oldPeers.get(address) match {
        case Some(value) => oldPeers.updated(address, value.copy(peerPriorityStatus = status))
        case None => oldPeers
      }
    })

  def updateHistoryComparisonResult(peer: InetSocketAddress, result: HistoryComparisonResult): ConnectedPeersCollection =
    ConnectedPeersCollection(peers.get(peer) match {
      case Some(value) => peers.updated(peer, value.copy(historyComparisonResult = result))
      case None => peers
    })

  def updateLastUptime(peer: InetSocketAddress): ConnectedPeersCollection =
    ConnectedPeersCollection(peers.get(peer) match {
      case Some(value) => peers.updated(peer, value.copy(lastUptime = LastUptime(System.currentTimeMillis())))
      case None => peers
    })

  def removePeer(address: InetSocketAddress): ConnectedPeersCollection = ConnectedPeersCollection(peers - address)

  def collect[T](p: (InetSocketAddress, PeerInfo) => Boolean,
                 f: (InetSocketAddress, PeerInfo) => T): Seq[T] = peers
    .collect { case (peer, info) if p(peer, info) => f(peer, info) }
    .toSeq
}

object ConnectedPeersCollection {

  final case class LastUptime(time: Long) extends AnyVal

  final case class PeerInfo(historyComparisonResult: HistoryComparisonResult,
                            peerPriorityStatus: PeersPriorityStatus,
                            connectedPeer: ConnectedPeer,
                            connectionType: ConnectionType,
                            lastUptime: LastUptime)

  def apply(): ConnectedPeersCollection = ConnectedPeersCollection(Map.empty[InetSocketAddress, PeerInfo])
}