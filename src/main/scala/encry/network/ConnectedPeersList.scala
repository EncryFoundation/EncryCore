package encry.network

import java.net.{InetAddress, InetSocketAddress}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{HistoryComparisonResult, Unknown}
import encry.network.ConnectedPeersList.{LastUptime, PeerInfo, PeersPriorityStatus}
import encry.network.ConnectedPeersList.PeersPriorityStatus.{InitialPriority, PeersPriorityStatus, Received, Requested}
import encry.network.PeerConnectionHandler.{ConnectedPeer, ConnectionType, Outgoing}
import encry.settings.EncryAppSettings

final class ConnectedPeersList(settings: EncryAppSettings) extends StrictLogging {

  private var peers: Map[InetAddress, PeerInfo] = Map.empty

  private var peersNetworkStatistic: Map[InetAddress, (Requested, Received)] = Map.empty

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

  def updatePeersPriorityStatus(): Unit = {
    peersNetworkStatistic.foreach { case (peer, (requested, received)) =>
      peers.get(peer) match {
        case Some(peerInfo) =>
          val priority: PeersPriorityStatus = PeersPriorityStatus.calculateStatuses(received, requested)
          logger.info(s"Updating priority status for peer $peer... $priority.")
          peers = peers.updated(peer, peerInfo.copy(peerPriorityStatus = priority))
        case None => logger.info(s"Can't update peer's $peer priority. No such peer in connected peers list.")
      }
    }
    peersNetworkStatistic = Map.empty[InetAddress, (Requested, Received)]
  }

  def incrementRequest(peer: InetAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = Requested(requested.increment)
    logger.info(s"Updating request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def incrementReceive(peer: InetAddress): Unit = {
    val (requested, received): (Requested, Received) = peers.getOrElse(peer, (Requested(), Received()))
    val newReceived: Received = Received(received.increment)
    logger.info(s"Updating received parameter from $peer. Old is ($requested, $received). New one is: ($requested, $newReceived)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (requested, newReceived))
  }

  def decrementRequest(peer: InetAddress): Unit = {
    val (requested, received): (Requested, Received) = peers.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = Requested(requested.decrement)
    logger.info(s"Decrement request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def incrementRequestForNModifiers(peer: InetAddress, modifiersQty: Int): Unit = {
    val (requested, received): (Requested, Received) = peers.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = Requested(requested.increment(modifiersQty))
    logger.info(s"Updating request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

//  def getPeersForConnection: IndexedSeq[(InetAddress, PeerInfo)] = peers
//    .filter { case (_, peerInfo) => peerInfo.historyComparisonResult != Fork }
//    .toIndexedSeq.sortBy { case (_, peerInfo) => peerInfo.peerPriorityStatus }

}

object ConnectedPeersList {

  final case class LastUptime(time: Long) extends AnyVal

  final case class PeerInfo(historyComparisonResult: HistoryComparisonResult,
                            peerPriorityStatus: PeersPriorityStatus,
                            connectedPeer: ConnectedPeer,
                            connectionType: ConnectionType,
                            lastUptime: LastUptime)

  object PeersPriorityStatus {

    sealed trait PeersPriorityStatus extends Any

    final case class HighPriority(priority: Int = 4) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: HighPriority"
    }

    final case class LowPriority(priority: Int = 3) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: LowPriority"
    }

    final case class InitialPriority(priority: Int = 2) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: InitialPriority"
    }

    final case class BadNode(priority: Int = 1) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: BadNodePriority"
    }

    final case class Received(received: Int = 0) extends AnyVal {
      def increment: Int = received + 1

      override def toString: String = s"Received: $received"
    }

    final case class Requested(requested: Int = 0) extends AnyVal {
      def increment: Int = requested + 1

      def decrement: Int = requested - 1

      def increment(qty: Int): Int = requested + qty

      override def toString: String = s"Requested: $requested"
    }

    private val criterionForHighP: Double = 0.75
    private val criterionForLowP: Double = 0.50

    def calculateStatuses(res: Received, req: Requested): PeersPriorityStatus =
      res.received.toDouble / req.requested match {
        case t if t >= criterionForHighP => HighPriority()
        case t if t >= criterionForLowP => LowPriority()
        case _ => BadNode()
      }
  }

}