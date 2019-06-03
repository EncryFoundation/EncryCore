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
    val unknowns: IndexedSeq[ConnectedPeer] = statuses.collect {
      case (_, (historyComparisonResult, _, cP)) if historyComparisonResult == Unknown => cP
    }.toIndexedSeq
    val olderNodes: IndexedSeq[ConnectedPeer] = statuses.collect {
      case (_, (historyComparisonResult, _, cP)) if historyComparisonResult == Older => cP
    }.toIndexedSeq
    val nonOutdated: IndexedSeq[ConnectedPeer] =
      if (olderNodes.nonEmpty) olderNodes(scala.util.Random.nextInt(olderNodes.size)) +: unknowns else unknowns
    val peers: Seq[ConnectedPeer] =
      if (outdated.nonEmpty) outdated
      else nonOutdated.filter(p => (System.currentTimeMillis() - lastSyncSentTime.getOrElse(p, 0L))
        .millis >= networkSettings.syncInterval)
    val resultedPeers: Seq[ConnectedPeer] =
      if (peers.nonEmpty) peers
      else statuses.map(_._2._3).toSeq
    resultedPeers.foreach(updateLastSyncSentTime)
    resultedPeers
  }
}