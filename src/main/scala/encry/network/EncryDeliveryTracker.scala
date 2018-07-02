package encry.network

import akka.actor.{Actor, ActorRef, Cancellable}
import encry.EncryApp.{networkController, settings}
import encry.network.EncryDeliveryTracker.{CheckForSync, CheckModifiersToDownload, FilteredModifiers, NeedToDownloadFromRandom}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.{CheckDelivery, RequestFromLocal}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, SendToNetwork}
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{Message, ModifiersSpec, RequestModifierSpec}
import encry.settings.{Algos, NetworkSettings}
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.NetworkTime.Time
import encry.utils.{NetworkTimeProvider, ScorexLogging}
import encry.view.history.EncryHistory
import encry.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class EncryDeliveryTracker(networkSettings: NetworkSettings, timeProvider: NetworkTimeProvider, nodeViewSynchronizer: ActorRef)
  extends Actor with ScorexLogging {

  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  protected type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  private val delivered = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  private val deliveredSpam = mutable.Map[ModifierIdAsKey, ConnectedPeer]()
  private val peers = mutable.Map[ModifierIdAsKey, Seq[ConnectedPeer]]()

  private val cancellables = mutable.Map[ModifierIdAsKey, (ConnectedPeer, (Cancellable, Int))]()

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId]): Unit = tryWithLogging {
    val notRequestedIds: Seq[ModifierId] = mids.foldLeft(Seq[ModifierId]()) {
      case (notRequested, modId) =>
        val modifierKey = key(modId)
        if (!cancellables.contains(modifierKey)) notRequested :+ modId
        else {
          peers(modifierKey) = (peers.getOrElseUpdate(modifierKey, Seq()) :+ cp).distinct
          notRequested
        }
    }
    if (notRequestedIds.nonEmpty) cp.handlerRef ! Message(requestModifierSpec, Right(mtid -> notRequestedIds), None)
    notRequestedIds.foreach{id =>
      val cancellable = context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout, self, CheckDelivery(cp, mtid, id))
      cancellables(key(id)) = cp -> (cancellable, 0)
    }
  }

  // stops expecting, and expects again if the number of checks does not exceed the maximum
  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId): Unit = tryWithLogging {

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
          expect(cp, mtid, Seq(mid))
        }
      )
    )
  }

  protected def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean = cancellables.contains(key(mid))

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

    case RequestFromLocal(peer, modifierTypeId, modifierIds) => expect(peer, modifierTypeId, modifierIds)

    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      if (settings.node.sendStat) context.actorSelection("/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => isSpam(id) }
      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Algos.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      if (fm.nonEmpty) {
        println(s"Send ${fm.values.size}")
        nodeViewSynchronizer ! FilteredModifiers(typeId, fm.values.toSeq)
      }

    case CheckDelivery(peer, modifierTypeId, modifierId) =>

      if (peerWhoDelivered(modifierId).contains(peer)) delete(modifierId)
      else {
        log.info(s"Peer $peer has not delivered asked modifier ${Algos.encode(modifierId)} on time")
        reexpect(peer, modifierTypeId, modifierId)
      }

    case CheckModifiersToDownload(history: EncryHistory) =>
      removeOutdatedExpectingFromRandom()
      if (history.isHeadersChainSynced && isExpectingFromRandom) {
        val currentQueue: Iterable[ModifierId] = expectingFromRandomQueue
        val newIds: Seq[(ModifierTypeId, ModifierId)] = history.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
        val oldIds: Seq[(ModifierTypeId, ModifierId)] = idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }

    case CheckForSync(history: EncryHistory) =>
      if (history.isHeadersChainSynced && isExpectingFromRandom) self ! CheckModifiersToDownload(history)

    case NeedToDownloadFromRandom(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]) =>
      requestDownload(modifierTypeId, modifierIds)
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    if (settings.node.sendStat) context.actorSelection("/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    networkController ! SendToNetwork(msg, SendToRandom)
  }

  case class ToDownloadStatus(tp: ModifierTypeId, firstViewed: Long, lastTry: Long)

  private val ToDownloadRetryInterval: FiniteDuration = 10.seconds
  private val ToDownloadLifetime: FiniteDuration = 1.hour

  // Modifiers we need to download, but do not know peer that have this modifier
  // TODO we may try to guess this peers using delivered map
  val expectingFromRandom: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()

  def isExpectingFromRandom: Boolean = expectingFromRandom.nonEmpty

  def isExpecting: Boolean = cancellables.nonEmpty

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
    if (expectingFromRandom.contains(key(mid))) {
      expectingFromRandom.remove(key(mid))
      delivered(key(mid)) = cp
    } else if (isExpecting(mtid, mid, cp)) {
      delivered(key(mid)) = cp
      cancellables.get(key(mid)).foreach(_._2._1.cancel())
      cancellables -= key(mid)
      peers -= key(mid)
    }
    else {
      deliveredSpam(key(mid)) = cp
    }
  }
}

object EncryDeliveryTracker {

  case class NeedToDownloadFromRandom(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

  case class CheckForSync(history: EncryHistory)

  case class CheckModifiersToDownload(history: EncryHistory)

  case class FilteredModifiers(typeId: ModifierTypeId, modifiers: Seq[Array[Byte]])
}