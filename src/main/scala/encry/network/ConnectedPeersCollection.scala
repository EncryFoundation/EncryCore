package encry.network

import java.net.InetSocketAddress
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.{HistoryComparisonResult, Unknown}
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

  def removePeer(address: InetSocketAddress): ConnectedPeersCollection = ConnectedPeersCollection(peers - address)

  def updatePriorityStatus(stats: Map[InetSocketAddress, PeersPriorityStatus]): ConnectedPeersCollection =
    ConnectedPeersCollection(updateK(stats, updateStatus))

  def updateHistoryComparisonResult(hcr: Map[InetSocketAddress, HistoryComparisonResult]): ConnectedPeersCollection =
    ConnectedPeersCollection(updateK(hcr, updateComparisonResult))

  def updateLastUptime(lup: Map[InetSocketAddress, LastUptime]): ConnectedPeersCollection =
    ConnectedPeersCollection(updateK(lup, updateUptime))

  def collect[T](p: (InetSocketAddress, PeerInfo) => Boolean,
                 f: (InetSocketAddress, PeerInfo) => T): Seq[T] = peers
    .collect { case (peer, info) if p(peer, info) => f(peer, info) }
    .toSeq

  def getAll: Map[InetSocketAddress, PeerInfo] = peers

  private def updateK[T](elems: Map[InetSocketAddress, T], f: (PeerInfo, T) => PeerInfo): Map[InetSocketAddress, PeerInfo] = {
    val newValue: Map[InetSocketAddress, PeerInfo] = for {
      (key, value) <- elems
      oldValue     <- peers.get(key)
    } yield key -> f(oldValue, value)
    peers ++ newValue
  }

  private def updateStatus: (PeerInfo, PeersPriorityStatus) => PeerInfo = (i, p) => i.copy(peerPriorityStatus = p)
  private def updateComparisonResult: (PeerInfo, HistoryComparisonResult) => PeerInfo = (i, h) => i.copy(historyComparisonResult = h)
  private def updateUptime: (PeerInfo, LastUptime) => PeerInfo = (i, u) => i.copy(lastUptime = u)

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