package encry.network

import akka.actor.{Actor, Cancellable}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.CheckDelivery
import encry.network.PeerConnectionHandler._
import encry.network.message.{Message, RequestModifierSpec}
import encry.settings.{Algos, NetworkSettings}
import encry.utils.NetworkTime.Time
import encry.utils.{NetworkTimeProvider, ScorexLogging}
import encry.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Try}

case class EncryDeliveryTracker(networkSettings: NetworkSettings, timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  protected type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  private val expecting = mutable.Set[(ModifierTypeId, ModifierIdAsKey, ConnectedPeer)]()
  private val delivered = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  private val deliveredSpam = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  private val peers = mutable.Map[ModifierIdAsKey, Seq[ConnectedPeer]]()

  private val cancellables = mutable.Map[ModifierIdAsKey, (ConnectedPeer, (Cancellable, Int))]()

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId])(implicit ec: ExecutionContext): Unit = tryWithLogging {
    for (mid <- mids) expect(cp, mtid, mid)
  }

  protected def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = {
    val midAsKey = key(mid)
    if (!cancellables.contains(midAsKey)) {
      cp.handlerRef ! Message(requestModifierSpec, Right(mtid -> Seq(mid)), None)
      val cancellable = context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout, self, CheckDelivery(cp, mtid, mid))
      expecting += ((mtid, midAsKey, cp))
      cancellable(midAsKey) = cp -> (cancellable, 0)
    }
    else peers(midAsKey) = (peers.getOrElseUpdate(midAsKey, Seq()) :+ cp).distinct
  }

  // stops expecting, and expects again if the number of checks does not exceed the maximum
  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId)(implicit ec: ExecutionContext): Unit = tryWithLogging {

    val midAsKey = key(mid)

    cancellables.get(midAsKey).foreach(peerInfo =>
      if (peerInfo._2._2 < networkSettings.maxDeliveryChecks) {
        cp.handlerRef ! Message(requestModifierSpec, Right(mtid -> Seq(mid)), None)
        val cancellable = context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout, self, CheckDelivery(cp, mtid, mid))
        peerInfo._2._1.cancel()
        cancellables(midAsKey) = cp -> (cancellable, peerInfo._2._2 + 1)
      } else peers.get(midAsKey).foreach(downloadPeers =>
        downloadPeers.headOption.foreach { nextPeer =>
          cancellables -= midAsKey
          peers(midAsKey) = downloadPeers.filter(_ != nextPeer)
          expect(cp, mtid, mid)
        }
      )
    )
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

  override def receive: Receive = {

    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (peerWhoDelivered(modifierId).contains(peer)) delete(modifierId)
      else {
        log.info(s"Peer $peer has not delivered asked modifier ${Algos.encode(modifierId)} on time")
        reexpect(peer, modifierTypeId, modifierId)
      }
  }

  case class ToDownloadStatus(tp: ModifierTypeId, firstViewed: Long, lastTry: Long)

  private val ToDownloadRetryInterval: FiniteDuration = 10.seconds
  private val ToDownloadLifetime: FiniteDuration = 1.hour

  // Modifiers we need to download, but do not know peer that have this modifier
  // TODO we may try to guess this peers using delivered map
  val expectingFromRandom: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()

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
    println("///////////////////////////////////////")
    expectingFromRandom.keys.foreach(key => Algos.encode(key.toArray))
    println(s"expectingFromRandom.contains(key(mid))[${Algos.encode(mid)}: ${expectingFromRandom.contains(key(mid))}")
    println(s"isExpecting(mtid, mid, cp): ${isExpecting(mtid, mid, cp)}")
    if (expectingFromRandom.contains(key(mid))) {
      expectingFromRandom.remove(key(mid))
      println(s"After remove: ${System.currentTimeMillis()}: ${expectingFromRandom.contains(key(mid))}")
      println(s"Size: ${expectingFromRandom.size}")
      delivered(key(mid)) = cp
    } else if (isExpecting(mtid, mid, cp)) {
      println("Here")
      val eo = expecting.find(e => (mtid == e._1) && (mid sameElements e._2) && cp == e._3)
      for (e <- eo) expecting -= e
      delivered(key(mid)) = cp
      cancellables.get(key(mid)).foreach(_._2._1.cancel())
      cancellables -= key(mid)
      peers -= key(mid)
    }
    else {
      println("222222")
      deliveredSpam(key(mid)) = cp
    }
    println("////////////////////////////////////////")
  }
}
