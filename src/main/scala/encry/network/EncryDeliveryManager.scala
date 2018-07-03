package encry.network

import akka.actor.{Actor, ActorRef, Cancellable}
import encry.EncryApp.{networkController, nodeViewHolder, settings}
import encry.consensus.History.{HistoryComparisonResult, Nonsense, Unknown, Younger}
import encry.network.EncryNodeViewSynchronizer.CheckModifiersToDownload
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, SendToNetwork}
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{InvSpec, Message, ModifiersSpec, RequestModifierSpec}
import encry.settings.NetworkSettings
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.NetworkTime.Time
import encry.utils.{EncryLogging, NetworkTimeProvider}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import encry.{ModifierId, ModifierTypeId}
import scorex.crypto.encode.Base58

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class EncryDeliveryManager(networkSettings: NetworkSettings,
                           nvsRef: ActorRef, timeProvider:
                           NetworkTimeProvider,
                           syncInfoSpec: EncrySyncInfoMessageSpec.type) extends Actor with EncryLogging {

  protected type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  protected def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  var historyReaderOpt: Option[EncryHistory] = None
  var mempoolReaderOpt: Option[EncryMempool] = None
  val invSpec: InvSpec = new InvSpec(networkSettings.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  val statusTracker: SyncTracker = SyncTracker(self, context, networkSettings, timeProvider)

  override def preStart(): Unit = {
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(settings.network.syncInterval, settings.network.syncInterval)(self ! CheckModifiersToDownload)
  }

  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    val peers: Seq[ConnectedPeer] = statusTracker.peersToSyncWith()
    if (peers.nonEmpty)
      networkController ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peers))
  }

  // todo: Do we need to keep track of ModifierTypeIds? Maybe we could ignore them?

  // when a remote peer is asked a modifier, we add the expected data to `expecting`
  // when a remote peer delivers expected data, it is removed from `expecting` and added to `delivered`.
  // when a remote peer delivers unexpected data, it is added to `deliveredSpam`.
  var delivered: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty[ModifierIdAsKey, ConnectedPeer]
  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty[ModifierIdAsKey, ConnectedPeer]
  var peers: Map[ModifierIdAsKey, Seq[ConnectedPeer]] = Map.empty[ModifierIdAsKey, Seq[ConnectedPeer]]

  var cancellables: Map[ModifierIdAsKey, (ConnectedPeer, (Cancellable, Int))] = Map.empty[ModifierIdAsKey, (ConnectedPeer, (Cancellable, Int))]

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId]): Unit = tryWithLogging {
    val notRequestedIds: Seq[ModifierId] = mids.foldLeft(Seq[ModifierId]()) {
      case (notRequested, modId) =>
        val modifierKey = key(modId)
        if (historyReaderOpt.forall(history => !history.contains(modId))) {
          if (!cancellables.contains(modifierKey)) notRequested :+ modId
          else {
            peers = peers - modifierKey + (modifierKey -> (peers.getOrElse(modifierKey, Seq()) :+ cp).distinct)
            notRequested
          }
        } else notRequested
    }
    if (notRequestedIds.nonEmpty) cp.handlerRef ! Message(requestModifierSpec, Right(mtid -> notRequestedIds), None)
    notRequestedIds.foreach { id =>
      val cancellable = context.system.scheduler.scheduleOnce(networkSettings.deliveryTimeout, self, CheckDelivery(cp, mtid, id))
      cancellables = (cancellables - key(id)) + (key(id) -> (cp, (cancellable, 0)))
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
        cancellables = (cancellables - midAsKey) + (midAsKey -> (cp -> (cancellable, peerInfo._2._2 + 1)))
      } else {
        cancellables -= midAsKey
        peers.get(midAsKey).foreach(downloadPeers =>
          downloadPeers.headOption.foreach { nextPeer =>
            peers = peers - midAsKey + (midAsKey -> downloadPeers.filter(_ != nextPeer))
            expect(nextPeer, mtid, Seq(mid))
          }
        )
      }
    )
  }

  protected def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean =
    cancellables.get(key(mid)).exists(_._1 == cp)

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => log.warn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  protected def tryWithLogging(fn: => Unit): Unit = {
    Try(fn).recoverWith {
      case e =>
        log.warn("Unexpected error", e)
        Failure(e)
    }
  }

  override def receive: Receive = {

    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => log.warn("Peer status is still unknown")
        case Nonsense => log.warn("Got nonsense")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (peerWhoDelivered(modifierId).contains(peer)) delete(modifierId)
      else reexpect(peer, modifierTypeId, modifierId)
    case CheckModifiersToDownload =>
      removeOutdatedExpectingFromRandom()
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = expectingFromRandomQueue
        val newIds: Seq[(ModifierTypeId, ModifierId)] = h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
        val oldIds: Seq[(ModifierTypeId, ModifierId)] = idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) expect(peer, modifierTypeId, modifierIds)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      if (settings.node.sendStat) context.actorSelection("akka://encry/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => isSpam(id) }
      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Base58.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      if (fm.nonEmpty) nodeViewHolder ! ModifiersFromRemote(remote, typeId, fm.values.toSeq)
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < (networkSettings.syncInterval.toMillis / 2)) log.debug("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] => historyReaderOpt = Some(reader)
    case ChangedMempool(reader: EncryMempool) if reader.isInstanceOf[EncryMempool] => mempoolReaderOpt = Some(reader)
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

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    if (settings.node.sendStat) context.actorSelection("akka://encry/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    statusTracker.peersToSyncWith().foreach(peer => {
      expect(peer, modifierTypeId, modifierIds)
      networkController ! SendToNetwork(msg, SendToPeer(peer))
    })
  }

  /**
    * Modifier downloaded
    */
  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    if (expectingFromRandom.contains(key(mid))) {
      expectingFromRandom.remove(key(mid))
      delivered = delivered - key(mid) + (key(mid) -> cp)
    } else if (isExpecting(mtid, mid, cp)) {
      delivered = delivered - key(mid) + (key(mid) -> cp)
      cancellables.get(key(mid)).foreach(_._2._1.cancel())
      cancellables = cancellables - key(mid)
      peers = peers - key(mid)
    }
    else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)
  }
}