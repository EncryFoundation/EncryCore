package encry.network

import akka.actor.{ActorContext, ActorRef, Cancellable}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.CheckDelivery
import encry.network.PeerConnectionHandler._
import encry.settings.Algos
import encry.utils.NetworkTime.Time
import encry.utils.{NetworkTimeProvider, ScorexLogging}
import encry.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Try}

case class EncryDeliveryTracker(context: ActorContext, deliveryTimeout: FiniteDuration, maxDeliveryChecks: Int,
                                nvsRef: ActorRef, timeProvider: NetworkTimeProvider) extends ScorexLogging {

  protected type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte
  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  // todo: Do we need to keep track of ModifierTypeIds? Maybe we could ignore them?

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  protected val expecting = mutable.Set[(ModifierTypeId, ModifierIdAsKey, ConnectedPeer)]()
  protected val delivered = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  protected val deliveredSpam = mutable.Map[ModifierIdAsKey, ConnectedPeer]()

  protected val cancellables = mutable.Map[(ModifierIdAsKey, ConnectedPeer), Cancellable]()
  protected val checksCounter = mutable.Map[(ModifierIdAsKey, ConnectedPeer), Int]()

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Unit = tryWithLogging {
    for (mid <- mids) expect(cp, mtid, mid)
  }

  protected def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = {
    val cancellable = context.system.scheduler.scheduleOnce(deliveryTimeout,
      nvsRef,
      CheckDelivery(cp, mtid, mid))
    val midAsKey = key(mid)
    expecting += ((mtid, midAsKey, cp))
    cancellables((midAsKey, cp)) = cancellable
  }

  // stops expecting, and expects again if the number of checks does not exceed the maximum
  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = tryWithLogging {
    stopExpecting(cp, mtid, mid)
    val midAsKey = key(mid)
    val checks = checksCounter.getOrElseUpdate((midAsKey, cp), 0) + 1
    checksCounter((midAsKey, cp)) = checks
    if (checks < maxDeliveryChecks) expect(cp, mtid, mid)
    else checksCounter -= ((midAsKey, cp))
  }

  protected def stopExpecting(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId): Unit = {
    val midAsKey = key(mid)
    expecting -= ((mtid, midAsKey, cp))
    cancellables((midAsKey, cp)).cancel()
    cancellables -= ((midAsKey, cp))
  }

  protected def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean =
    expecting.exists(e => (mtid == e._1) && (mid sameElements e._2.array) && cp == e._3)

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  protected def tryWithLogging(fn: => Unit): Unit = {
    Try(fn).recoverWith {
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
  }

  case class ToDownloadStatus(tp: ModifierTypeId, firstViewed: Long, lastTry: Long)

  private val ToDownloadRetryInterval: FiniteDuration = 10.seconds
  private val ToDownloadLifetime: FiniteDuration = 1.hour

  // Modifiers we need to download, but do not know peer that have this modifier
  // TODO we may try to guess this peers using delivered map
  private val expectingFromRandom: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()

  def isExpectingFromRandom: Boolean = expectingFromRandom.nonEmpty

  def isExpecting: Boolean = expecting.nonEmpty

  /**
    * @return ids we're going to download
    */
  def expectingFromRandomQueue: Iterable[ModifierId] = ModifierId @@ expectingFromRandom.keys.map(_.array)

  /**
    * Process download request of modifier of type modifierTypeId with id modifierId
    */
  def expectFromRandom(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val downloadRequestTime: Time = timeProvider.time()
    val newValue: ToDownloadStatus = expectingFromRandom.get(key(modifierId))
      .map(_.copy(lastTry = downloadRequestTime))
      .getOrElse(ToDownloadStatus(modifierTypeId, downloadRequestTime, downloadRequestTime))
    expectingFromRandom.put(key(modifierId), newValue)
  }

  /**
    * Remove old modifiers from download queue
    */
  def removeOutdatedExpectingFromRandom(): Unit = expectingFromRandom
      .filter { case (_, status) => status.firstViewed < timeProvider.time() - ToDownloadLifetime.toMillis }
      .foreach { case (key, _) => expectingFromRandom.remove(key) }

  /**
    * Id's that are already in queue to download but are not downloaded yet and were not requested recently
    */
  def idsExpectingFromRandomToRetry(): Seq[(ModifierTypeId, ModifierId)] = expectingFromRandom
      .filter(_._2.lastTry < timeProvider.time() - ToDownloadRetryInterval.toMillis).toSeq
      .sortBy(_._2.lastTry)
      .map(i => (i._2.tp, ModifierId @@ i._1.array))

  /**
    * Modifier downloaded
    */
  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    expectingFromRandom.keys.foreach(key => println(Algos.encode(key.asInstanceOf[Array[Byte]])))
      if (isExpecting(mtid, mid, cp)) {
        val eo = expecting.find(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3)
        for (e <- eo) expecting -= e
        delivered(key(mid)) = cp
        val cancellableKey = (key(mid), cp)
        for (c <- cancellables.get(cancellableKey)) c.cancel()
        cancellables -= cancellableKey
      }
      else {
        deliveredSpam(key(mid)) = cp
      }
    }
}
