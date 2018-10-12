package encry.network

import akka.actor.{Actor, Cancellable}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.EncryApp.{networkController, nodeViewHolder, settings}
import encry.consensus.History.{HistoryComparisonResult, Older, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManager.{ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.DeliveryManager.{ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, SendToNetwork}
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{InvSpec, Message, ModifiersSpec, RequestModifierSpec}
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.EncryMempool
import encry.utils.Logging
import org.encryfoundation.common.Algos
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}

class DeliveryManager extends Actor with Logging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var delivered: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var peers: Map[ModifierIdAsKey, Seq[ConnectedPeer]] = Map.empty
  var cancellables: Map[ModifierIdAsKey, (ConnectedPeer, (Cancellable, Int))] = Map.empty
  var mempoolReaderOpt: Option[EncryMempool] = None
  var historyReaderOpt: Option[EncryHistory] = None
  var isBlockChainSynced: Boolean = false
  var isMining: Boolean = settings.node.mining
  val invSpec: InvSpec = new InvSpec(settings.network.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
  val statusTracker: SyncTracker = SyncTracker(self, context, settings.network)

  def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  override def preStart(): Unit = {
    statusTracker.scheduleSendSyncInfo()
    self ! SendLocalSyncInfo
    context.system.scheduler
      .schedule(settings.network.modifierDeliverTimeCheck, settings.network.syncInterval)(self ! CheckModifiersToDownload)
  }

  override def receive: Receive = syncCycle

  def syncCycle: Receive = syncSending orElse netMessages

  def syncSending: Receive = {
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < settings.network.syncInterval.toMillis / 2)
        logInfo("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case StopSync => context.become(netMessages)
  }

  def netMessages: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => logInfo("Peer status is still unknown")
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
        val newIds: Seq[(ModifierTypeId, ModifierId)] =
          h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
            .filter(modId => !cancellables.keySet.contains(key(modId._2)))
        if (newIds.nonEmpty) newIds.groupBy(_._1).foreach {
          case (modId: ModifierTypeId, ids: Seq[(ModifierTypeId, ModifierId)]) => requestDownload(modId, ids.map(_._2))
        } else context.become(syncCycle)
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty && modifierTypeId != 2) expect(peer, modifierTypeId, modifierIds)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => isSpam(id) }
      if (settings.influxDB.isDefined)
        context.actorSelection("/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      if (spam.nonEmpty) {
        logInfo(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Algos.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      fm.values.foreach(modifierSer => nodeViewHolder ! ModifiersFromRemote(typeId, Seq(modifierSer)))
      historyReaderOpt.foreach { h =>
        if (!h.isHeadersChainSynced && cancellables.isEmpty) sendSync(h.syncInfo)
        else if (h.isHeadersChainSynced && !h.isFullChainSynced && cancellables.isEmpty) self ! CheckModifiersToDownload
      }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
    case FullBlockChainSynced => isBlockChainSynced = true
    case StartMining => isMining = true
    case DisableMining => isMining = false
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < settings.network.syncInterval.toMillis / 2)
        logInfo("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      historyReaderOpt = Some(reader)
    case ChangedMempool(reader: EncryMempool) if reader.isInstanceOf[EncryMempool] => mempoolReaderOpt = Some(reader)
    case ContinueSync =>
      context.become(syncCycle)
      self ! SendLocalSyncInfo
  }

  def sendSync(syncInfo: EncrySyncInfo): Unit = statusTracker.peersToSyncWith().foreach(peer =>
    peer.handlerRef ! Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
  )

  def expect(peer: ConnectedPeer, mTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = tryWithLogging {
    if ((mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining) || mTypeId != Transaction.ModifierTypeId) {
      val notYetRequestedIds: Seq[ModifierId] = modifierIds.foldLeft(Vector[ModifierId]()) {
        case (notYetRequested, modId) =>
          val modifierKey: ModifierIdAsKey = key(modId)
          if (historyReaderOpt.forall(history => !history.contains(modId) && !delivered.contains(key(modId)))) {
            if (!cancellables.contains(modifierKey)) notYetRequested :+ modId
            else {
              peers = peers.updated(modifierKey, (peers.getOrElse(modifierKey, Seq()) :+ peer).distinct)
              notYetRequested
            }
          } else notYetRequested
      }
      if (notYetRequestedIds.nonEmpty) peer.handlerRef ! Message(requestModifierSpec, Right(mTypeId -> notYetRequestedIds), None)
      notYetRequestedIds.foreach { id =>
        val cancellable: Cancellable = context.system.scheduler
          .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, id))
        cancellables = cancellables.updated(key(id), (peer, (cancellable, 0)))
      }
    }
  }

  def reexpect(cp: ConnectedPeer, mTypeId: ModifierTypeId, modifierId: ModifierId): Unit = tryWithLogging {
    val modifierKey: ModifierIdAsKey = key(modifierId)
    val peerAndHistoryOpt: Option[(ConnectedPeer, HistoryComparisonResult)] =
      statusTracker.statuses.find(peer => peer._1.socketAddress == cp.socketAddress)
    cancellables.get(modifierKey) match {
      case Some(peerInfo) if peerInfo._2._2 < settings.network.maxDeliveryChecks && peerAndHistoryOpt.isDefined =>
        peerAndHistoryOpt.foreach { case (peer, _) =>
          logDebug(s"Re-ask ${cp.socketAddress} and handler: ${cp.handlerRef} for modifiers of type: $mTypeId with id: " +
            s"${Algos.encode(modifierId)}")
          peer.handlerRef ! Message(requestModifierSpec, Right(mTypeId -> Seq(modifierId)), None)
          val cancellable: Cancellable = context.system.scheduler
            .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mTypeId, modifierId))
          peerInfo._2._1.cancel()
          cancellables = cancellables.updated(modifierKey, peer -> (cancellable, peerInfo._2._2 + 1))
        }
      case Some(_) =>
        cancellables -= modifierKey
        peers.get(modifierKey).foreach { downloadPeers =>
          downloadPeers.headOption.foreach { nextPeer =>
            peers = peers.updated(modifierKey, downloadPeers.filter(_ != nextPeer))
            expect(nextPeer, mTypeId, Seq(modifierId))
          }
        }
      case None =>
    }
  }

  def isExpecting(mtid: ModifierTypeId, mid: ModifierId): Boolean = cancellables.contains(key(mid))

  def delete(mid: ModifierId): Unit = tryWithLogging(delivered -= key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) tryWithLogging(deliveredSpam -= key(id))

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def peerWhoDelivered(mid: ModifierId): Option[ConnectedPeer] = delivered.get(key(mid))

  def sendExtension(remote: ConnectedPeer, status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => logInfo(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  def tryWithLogging(fn: => Unit): Unit = Try(fn).recoverWith {
    case e => logInfo(s"Unexpected error: $e")
      Failure(e)
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    if (settings.influxDB.isDefined)
      context.actorSelection("/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    statusTracker.statuses.keys.foreach(expect(_, modifierTypeId, modifierIds))
  }

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    if (isExpecting(mtid, mid)) {
      delivered = delivered.updated(key(mid), cp)
      cancellables.get(key(mid)).foreach(_._2._1.cancel())
      cancellables -= key(mid)
      peers -= key(mid)
    }
    else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)
  }
}

object DeliveryManager {

  case object FullBlockChainSynced

  case object StopSync

  case object ContinueSync

}