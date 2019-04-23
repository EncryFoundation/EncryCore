package encry.network

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{ActorContext, ActorRef, Cancellable}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.network.NodeViewSynchronizer.ReceivableMessages.SendLocalSyncInfo
import encry.network.PeerConnectionHandler._
import encry.network.SyncTracker.PeerPriorityStatus
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.settings.NetworkSettings
import encry.utils.NetworkTime.Time
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class SyncTracker(deliveryManager: ActorRef,
                       context: ActorContext,
                       networkSettings: NetworkSettings) extends StrictLogging {

  var statuses: Map[InetAddress, (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer)] = Map.empty

  private var schedule: Option[Cancellable] = None
  private val lastSyncSentTime: mutable.Map[ConnectedPeer, Time] = mutable.Map[ConnectedPeer, Time]()
  private var lastSyncInfoSentTime: Time = 0L

  /**
    * Collection contains statistics of communication with peers.
    *
    * Key - peer address which we connecting with.
    *
    * Value - tuple(Requested, Received).
    * Value shows, how many modifiers has been requested and received during the current period.
    */

  private type Requested = Int
  private type Received  = Int
  private var peersNetworkCommunication: Map[InetAddress, (Requested, Received)] = Map.empty

  def updatePeersPriorityStatus(): Unit = {
    peersNetworkCommunication.foreach { case (peer, (requested, received)) =>
      statuses.get(peer) match {
        case Some((hcr, _, cp)) =>
          val priority: PeerPriorityStatus = PeerPriorityStatus.definePriorityStatus(requested, received)
          logger.info(s"Peer $peer has new priority: ${PeerPriorityStatus.toString(priority)}.")
          statuses = statuses.updated(peer, (hcr, priority, cp))
        case None => logger.info(s"Can't update peer $peer priority. No such peer in status tracker")
      }
    }
    peersNetworkCommunication = Map.empty[InetAddress, (Requested, Received)]
  }

  def incrementRequest(peer: ConnectedPeer): Unit = {
    val requestReceiveStat: (Requested, Received) = peersNetworkCommunication.getOrElse(peer.socketAddress.getAddress, (0, 0))
    logger.debug(s"Updating request parameter from ${peer.socketAddress}. Old is $requestReceiveStat." +
      s" New one is: (${requestReceiveStat._1 + 1}, ${requestReceiveStat._2})")
    peersNetworkCommunication =
      peersNetworkCommunication.updated(peer.socketAddress.getAddress, (requestReceiveStat._1 + 1, requestReceiveStat._2))
  }

  def incrementRequestForNModifiers(peer: ConnectedPeer, modifiersQty: Int): Unit = {
    val requestReceiveStat: (Requested, Received) = peersNetworkCommunication.getOrElse(peer.socketAddress.getAddress, (0, 0))
    logger.debug(s"Updating request parameter from ${peer.socketAddress}. Old is $requestReceiveStat." +
      s" New one is: (${requestReceiveStat._1 + modifiersQty}, ${requestReceiveStat._2})")
    peersNetworkCommunication =
      peersNetworkCommunication.updated(peer.socketAddress.getAddress, (requestReceiveStat._1 + modifiersQty, requestReceiveStat._2))
  }

  def incrementReceive(peer: ConnectedPeer): Unit = {
    val requestReceiveStat: (Requested, Received) = peersNetworkCommunication.getOrElse(peer.socketAddress.getAddress, (0, 0))
    logger.debug(s"Updating received parameter from ${peer.socketAddress}. Old is $requestReceiveStat." +
      s" New one is: (${requestReceiveStat._1}, ${requestReceiveStat._2 + 1})")
    peersNetworkCommunication =
      peersNetworkCommunication.updated(peer.socketAddress.getAddress, (requestReceiveStat._1, requestReceiveStat._2 + 1))
  }

  def decrementRequest(peer: ConnectedPeer): Unit = {
    val requestReceiveStat: (Requested, Received) = peersNetworkCommunication.getOrElse(peer.socketAddress.getAddress, (0, 0))
    logger.debug(s"Decrement request parameter from ${peer.socketAddress}. Old is $requestReceiveStat." +
      s" New one is: (${requestReceiveStat._1}, ${requestReceiveStat._2 - 1})")
    peersNetworkCommunication =
      peersNetworkCommunication.updated(peer.socketAddress.getAddress, (requestReceiveStat._1, requestReceiveStat._2 - 1))
  }

  def getPeersForConnection: Vector[(InetAddress, (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer))] =
    statuses
      .filter { case (_, (hcr, _, _)) => hcr != Younger }
      .toVector.sortBy { case (_, (_, pps, _)) => pps }

  def scheduleSendSyncInfo(): Unit = {
    schedule.foreach(_.cancel())
    schedule = Some(context.system.scheduler.schedule(
      networkSettings.syncInterval, networkSettings.syncInterval)(deliveryManager ! SendLocalSyncInfo)
    )
  }

  def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult): Unit = {
    val priority: PeerPriorityStatus =
      statuses.getOrElse(peer.socketAddress.getAddress, (Unknown, PeerPriorityStatus.InitialPriority, peer))._2
    val seniorsBefore: Int = numberOfOlderNodes
    statuses = statuses.updated(peer.socketAddress.getAddress, (status, priority, peer))
    val olderAfter: Int = numberOfOlderNodes
    if (seniorsBefore > 0 && olderAfter == 0) {
      logger.info("Syncing is done, switching to stable regime")
      scheduleSendSyncInfo()
    }
  }

  def clearStatus(remote: InetSocketAddress): Unit = {
    statuses.keys.find(_ == remote.getAddress) match {
      case Some(peer) => statuses -= peer
      case None => logger.warn(s"Trying to clear status for $remote, but it is not found")
    }
    lastSyncSentTime.keys.find(_.socketAddress.getAddress == remote.getAddress) match {
      case Some(peer) => lastSyncSentTime -= peer
      case None => logger.warn(s"Trying to clear last sync time for $remote, but it is not found")
    }
  }

  def updateLastSyncSentTime(peer: ConnectedPeer): Unit = {
    val currentTime: Time = System.currentTimeMillis()
    lastSyncSentTime(peer) = currentTime
    lastSyncInfoSentTime = currentTime
  }

  def elapsedTimeSinceLastSync: Long = System.currentTimeMillis() - lastSyncInfoSentTime

  private def outdatedPeers: Seq[ConnectedPeer] =
    lastSyncSentTime.filter(t => (System.currentTimeMillis() - t._2).millis > networkSettings.syncInterval).keys.toSeq

  private def numberOfOlderNodes: Int = statuses.count(_._2._1 == Older)

  /**
    * Return the peers to which this node should send a sync signal, including:
    * outdated peers, if any, otherwise, all the peers with unknown status plus a random peer with
    * `Older` status.
    */
  def peersToSyncWith: Seq[ConnectedPeer] = {
    val outdated: Seq[ConnectedPeer] = outdatedPeers
    lazy val unknowns: IndexedSeq[ConnectedPeer] = statuses.collect{
      case (_, (historyComparisonResult, _, cP)) if historyComparisonResult == Unknown => cP
    }.toIndexedSeq
    lazy val olderNodes: IndexedSeq[ConnectedPeer] = statuses.collect{
      case (_, (historyComparisonResult, _, cP)) if historyComparisonResult == Older => cP
    }.toIndexedSeq
    lazy val nonOutdated: IndexedSeq[ConnectedPeer] =
      if (olderNodes.nonEmpty) olderNodes(scala.util.Random.nextInt(olderNodes.size)) +: unknowns else unknowns
    val peers: Seq[ConnectedPeer] = if (outdated.nonEmpty) outdated
    else nonOutdated.filter(p => (System.currentTimeMillis() - lastSyncSentTime.getOrElse(p, 0L))
      .millis >= networkSettings.syncInterval)
    peers.foreach(updateLastSyncSentTime)
    peers
  }
}

object SyncTracker {

  object PeerPriorityStatus {

    type PeerPriorityStatus = Int

    val HighPriority: PeerPriorityStatus    = 4
    val LowPriority: PeerPriorityStatus     = 3
    val InitialPriority: PeerPriorityStatus = 2
    val BadNode: PeerPriorityStatus         = 1

    private val criterionForHighP: Double = 0.75
    private val criterionForLowP: Double  = 0.50

    def definePriorityStatus(requested: Int, received: Int): PeerPriorityStatus =
      received.toDouble / requested match {
        case t if t >= criterionForHighP => HighPriority
        case t if t >= criterionForLowP  => LowPriority
        case _                           => BadNode
      }

    def toString(priority: PeerPriorityStatus): String = priority match {
      case 1 => "BadNode"
      case 2 => "InitialPriorityNode"
      case 3 => "LowPriorityNode"
      case 4 => "HighPriorityNode"
    }
  }

}