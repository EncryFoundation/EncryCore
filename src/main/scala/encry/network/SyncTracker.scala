package encry.network

import java.net.InetSocketAddress
import akka.actor.{ActorContext, ActorRef, Cancellable}
import encry.consensus.History._
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SendLocalSyncInfo
import encry.network.PeerConnectionHandler._
import encry.settings.NetworkSettings
import encry.utils.Logging
import encry.utils.NetworkTime.Time
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class SyncTracker(deliveryManager: ActorRef,
                       context: ActorContext,
                       networkSettings: NetworkSettings) extends Logging {

  var statuses: Map[ConnectedPeer, HistoryComparisonResult] = Map.empty
  private var schedule: Option[Cancellable] = None
  private val lastSyncSentTime: mutable.Map[ConnectedPeer, Time] = mutable.Map[ConnectedPeer, Time]()
  private var lastSyncInfoSentTime: Time = 0L

  def scheduleSendSyncInfo(): Unit = {
    schedule.foreach(_.cancel())
    schedule = Some(context.system.scheduler.schedule(
      networkSettings.modifierDeliverTimeCheck, networkSettings.syncInterval)(deliveryManager ! SendLocalSyncInfo)
    )
  }

  def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult): Unit = {
    val seniorsBefore: Int = numOfSeniors()
    statuses = statuses.updated(peer, status)
    val seniorsAfter: Int = numOfSeniors()
    if (seniorsBefore > 0 && seniorsAfter == 0) {
      logInfo("Syncing is done, switching to stable regime")
      scheduleSendSyncInfo()
    }
  }

  def clearStatus(remote: InetSocketAddress): Unit = {
    statuses.keys.find(_.socketAddress == remote) match {
      case Some(peer) => statuses -= peer
      case None => logWarn(s"Trying to clear status for $remote, but it is not found")
    }
    lastSyncSentTime.keys.find(_.socketAddress.getAddress == remote.getAddress) match {
      case Some(peer) => lastSyncSentTime -= peer
      case None => logWarn(s"Trying to clear last sync time for $remote, but it is not found")
    }
  }

  def updateLastSyncSentTime(peer: ConnectedPeer): Unit = {
    val currentTime: Time = System.currentTimeMillis()
    lastSyncSentTime(peer) = currentTime
    lastSyncInfoSentTime = currentTime
  }

  def elapsedTimeSinceLastSync(): Long = System.currentTimeMillis() - lastSyncInfoSentTime

  private def outdatedPeers(): Seq[ConnectedPeer] =
    lastSyncSentTime.filter(t => (System.currentTimeMillis() - t._2).millis > networkSettings.syncInterval).keys.toSeq

  private def numOfSeniors(): Int = statuses.count(_._2 == Older)

  /**
    * Return the peers to which this node should send a sync signal, including:
    * outdated peers, if any, otherwise, all the peers with unknown status plus a random peer with
    * `Older` status.
    */
  def peersToSyncWith(): Seq[ConnectedPeer] = {
    val outdated: Seq[ConnectedPeer] = outdatedPeers()
    lazy val unknowns: IndexedSeq[ConnectedPeer] = statuses.filter(_._2 == Unknown).keys.toIndexedSeq
    lazy val olders: IndexedSeq[ConnectedPeer] = statuses.filter(_._2 == Older).keys.toIndexedSeq
    lazy val nonOutdated: IndexedSeq[ConnectedPeer] =
      if (olders.nonEmpty) olders(scala.util.Random.nextInt(olders.size)) +: unknowns else unknowns
    val peers: Seq[ConnectedPeer] = if (outdated.nonEmpty) outdated
    else nonOutdated.filter(p => (System.currentTimeMillis() - lastSyncSentTime.getOrElse(p, 0L))
      .millis >= networkSettings.syncInterval)
    peers.foreach(updateLastSyncSentTime)
    logDebug(s"Trying to get nodes to sync and they are: ${peers.map(_.socketAddress).mkString(",")} and " +
      s"handler are: ${peers.map(_.handlerRef).mkString(",")}")
    peers
  }
}
