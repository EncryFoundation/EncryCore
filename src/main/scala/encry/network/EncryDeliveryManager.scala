package encry.network

import akka.actor.{Actor, Cancellable}
import encry.EncryApp.{networkController, nodeViewHolder, settings, timeProvider}
import encry.consensus.History.{HistoryComparisonResult, Nonsense, Unknown, Younger}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, SendToNetwork}
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{InvSpec, Message, ModifiersSpec, RequestModifierSpec}
import encry.settings.Algos
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import encry.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}

class EncryDeliveryManager(syncInfoSpec: EncrySyncInfoMessageSpec.type) extends Actor with Logging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  case class ToDownloadStatus(modTypeId: ModifierTypeId, firstViewed: Long, lastTry: Long)

  var delivered: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var peers: Map[ModifierIdAsKey, Seq[ConnectedPeer]] = Map.empty
  var cancellables: Map[ModifierIdAsKey, (ConnectedPeer, (Cancellable, Int))] = Map.empty
  var mempoolReaderOpt: Option[EncryMempool] = None
  var historyReaderOpt: Option[EncryHistory] = None
  val invSpec: InvSpec = new InvSpec(settings.network.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
  val statusTracker: SyncTracker = SyncTracker(self, context, settings.network, timeProvider)

  def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  override def preStart(): Unit = {
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(settings.network.syncInterval, settings.network.syncInterval)(self ! CheckModifiersToDownload)
  }

  override def receive: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => log.info("Peer status is still unknown")
        case Nonsense => log.info("Got nonsense")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (peerWhoDelivered(modifierId).contains(peer)) delete(modifierId)
      else reexpect(peer, modifierTypeId, modifierId)
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = cancellables.keys.map(ModifierId @@ _.toArray)
        val newIds: Seq[(ModifierTypeId, ModifierId)] = h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
        newIds.groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) => if (modifierIds.nonEmpty && modifierTypeId != 2) expect(peer, modifierTypeId, modifierIds)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => isSpam(id) }
      if (settings.node.sendStat) context.actorSelection("akka://encry/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Algos.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      if (fm.nonEmpty) nodeViewHolder ! ModifiersFromRemote(remote, typeId, fm.values.toSeq)
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < (settings.network.syncInterval.toMillis / 2)) log.info("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] => historyReaderOpt = Some(reader)
  }

  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    val peers: Seq[ConnectedPeer] = statusTracker.peersToSyncWith()
    if (peers.nonEmpty)
      networkController ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peers))
  }

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId]): Unit = tryWithLogging {
    val notRequestedIds: Seq[ModifierId] = mids.foldLeft(Seq[ModifierId]()) {
      case (notRequested, modId) =>
        val modifierKey: ModifierIdAsKey = key(modId)
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
      logger.info(s"Request for ${Algos.encode(id)}")
      val cancellable: Cancellable = context.system.scheduler.scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mtid, id))
      cancellables = (cancellables - key(id)) + (key(id) -> (cp, (cancellable, 0)))
    }
  }

  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId): Unit = tryWithLogging {
    val midAsKey: ModifierIdAsKey = key(mid)
    cancellables.get(midAsKey).foreach(peerInfo =>
      if (peerInfo._2._2 < settings.network.maxDeliveryChecks) {
        cp.handlerRef ! Message(requestModifierSpec, Right(mtid -> Seq(mid)), None)
        val cancellable: Cancellable = context.system.scheduler.scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mtid, mid))
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

  def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean = cancellables.get(key(mid)).exists(_._1 == cp)

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  def sendExtension(remote: ConnectedPeer, status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => log.info(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  def tryWithLogging(fn: => Unit): Unit = {
    Try(fn).recoverWith {
      case e =>
        log.info("Unexpected error", e)
        Failure(e)
    }
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    if (settings.node.sendStat) context.actorSelection("akka://encry/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    statusTracker.peersToSyncWith().foreach(peer => {
      expect(peer, modifierTypeId, modifierIds)
      networkController ! SendToNetwork(msg, SendToPeer(peer))
    })
  }

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    if (isExpecting(mtid, mid, cp)) {
      logger.info(s"Get ${Algos.encode(mid)}")
      delivered = delivered - key(mid) + (key(mid) -> cp)
      cancellables.get(key(mid)).foreach(_._2._1.cancel())
      cancellables -= key(mid)
      peers -= key(mid)
    }
    else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)
  }
}