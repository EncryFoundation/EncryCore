package encry.network

import akka.actor.{Actor, Cancellable}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.EncryApp.{networkController, nodeViewHolder, settings}
import encry.consensus.History.{HistoryComparisonResult, Older, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.DeliveryManager.{ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.DeliveryManager.{CleanDelivered, ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
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
    case HandshakedPeer(remote) =>
      logInfo(s"Get peer: $remote")
      statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      logInfo(s"Going to check delivery of ${Algos.encode(modifierId)} with type: $modifierTypeId from peer: $peer")
      if (peerWhoDelivered(modifierId).contains(peer)) {
        logInfo("We get this modifier. So delete it from delivered")
        delete(modifierId)
      }
      else {
        logInfo("Reexpect it")
        reexpect(peer, modifierTypeId, modifierId)
      }
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = cancellables.keys.map(ModifierId @@ _.toArray)
        val newIds: Seq[(ModifierTypeId, ModifierId)] =
          h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
            .filter(modId => !cancellables.keySet.contains(key(modId._2)))
        logInfo(s"After filter newIds: ${newIds.map(mod => Algos.encode(mod._2)).mkString(",")}")
        if (newIds.nonEmpty) newIds.groupBy(_._1).foreach {
          case (modId: ModifierTypeId, ids: Seq[(ModifierTypeId, ModifierId)]) => requestDownload(modId, ids.map(_._2))
        } else context.become(syncCycle)
      }
//    case CleanDelivered(modifiersInCache: Seq[mutable.WrappedArray[Byte]]) =>
//      logInfo("Get clean delivered")
//      logInfo(s"Cache contains: ${modifiersInCache.map(mod => Algos.encode(mod.toArray)).mkString(",")}")
//      logInfo(s"Delivered contains: ${delivered.keys.map(mod => Algos.encode(mod.toArray)).mkString(",")}")
//      val newDelivered: Seq[ModifierIdAsKey] =
//        delivered.keys.filter(modId =>
//          historyReaderOpt.exists(_.contains(ModifierId @@ modId.toArray)) ||
//            modifiersInCache.contains(modId)).toSeq
//      logInfo(s"NewDelivered contains: ${newDelivered.map(mod => Algos.encode(mod.toArray)).mkString(",")}")
//      delivered = delivered.filter(mod => newDelivered.contains(mod._1))
//      logInfo(s"Delivered contains after filter: ${delivered.keys.map(key => Algos.encode(key.toArray)).mkString(",")}")
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty && modifierTypeId != 2) expect(peer, modifierTypeId, modifierIds)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => isSpam(id) }
      if (settings.influxDB.isDefined)
        context.actorSelection("/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      logInfo(s"Got modifiers (${modifiers.size}): ${modifiers.keys.map(Algos.encode).mkString(",")} " +
        s"of type $typeId from remote connected peer: $remote")
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      if (spam.nonEmpty) {
        logInfo(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Algos.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      logInfo(s"fm contains: ${fm.keys.map(Algos.encode).mkString(",")}")
      if (fm.nonEmpty) nodeViewHolder ! ModifiersFromRemote(typeId, fm.values.toSeq)
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

  def expect(cp: ConnectedPeer, mtid: ModifierTypeId, mids: Seq[ModifierId]): Unit = tryWithLogging {
    logInfo(s"Going to expect delivery modifiers of type $mids from $cp: ${mids.map(Algos.encode).mkString(",")}")
    if ((mtid == 2 && isBlockChainSynced && isMining) || mtid != 2) {
      val notRequestedIds: Seq[ModifierId] = mids.foldLeft(Seq[ModifierId]()) {
        case (notRequested, modId) =>
          val modifierKey: ModifierIdAsKey = key(modId)
          if (historyReaderOpt.forall(history => {
            logInfo(s"History contains ${Algos.encode(modId)}: ${history.contains(modId)}")
            logInfo(s"Delivered contains ${Algos.encode(modId)}: ${delivered.contains(key(modId))}")
            !history.contains(modId) && !delivered.contains(key(modId))
          })) {
            logInfo(s"cancellables.contains(${Algos.encode(modId)}): ${cancellables.contains(modifierKey)}")
            if (!cancellables.contains(modifierKey)) notRequested :+ modId
            else {
              logInfo("Add peer to peers")
              peers = peers.updated(modifierKey, (peers.getOrElse(modifierKey, Seq()) :+ cp).distinct)
              logInfo(s"Update peers. Now: ${peers.map(modifier => s"Can download ${Algos.encode(modifier._1.toArray)} " +
                s"from ${modifier._2.map(_.toString).mkString(",")}")}")
              notRequested
            }
          } else notRequested
      }
      logInfo(s"notRequestedIds.nonEmpty: ${notRequestedIds.nonEmpty}")
      if (notRequestedIds.nonEmpty) {
        logInfo(s"Send modifier request of: ${notRequestedIds.map(Algos.encode).mkString(",")} to $cp")
        cp.handlerRef ! Message(requestModifierSpec, Right(mtid -> notRequestedIds), None)
      }

      notRequestedIds.foreach { id =>
        val cancellable: Cancellable = context.system.scheduler
          .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mtid, id))
        cancellables = cancellables.updated(key(id), (cp, (cancellable, 0)))
        logInfo(s"Update cancellables! Now contains: ${cancellables.keys.map(key => Algos.encode(key.toArray)).mkString(",")}")
      }
    } else {
      logInfo("")
    }
  }

  def reexpect(cp: ConnectedPeer, mtid: ModifierTypeId, mid: ModifierId): Unit = tryWithLogging {
    val midAsKey: ModifierIdAsKey = key(mid)
    logInfo(s"cancellables.get(midAsKey): ${cancellables.getOrElse(midAsKey, None)}")
    cancellables.get(midAsKey).foreach { peerInfo =>
      logInfo(s"peerInfo._2._2: ${peerInfo._2._2}")
      logInfo(s"statusTracker.statuses.exists(peer =>peer._1.socketAddress == cp.socketAddress): ${statusTracker.statuses.exists(peer =>
        peer._1.socketAddress == cp.socketAddress)}")
      if (peerInfo._2._2 < settings.network.maxDeliveryChecks && statusTracker.statuses.exists(peer =>
        peer._1.socketAddress == cp.socketAddress)) {
        statusTracker.statuses.find(peer => peer._1.socketAddress == cp.socketAddress).foreach { peer =>
          logDebug(s"Re-ask ${cp.socketAddress} and handler: ${cp.handlerRef} for modifiers of type: $mtid with id: " +
            s"${Algos.encode(mid)}")
          peer._1.handlerRef ! Message(requestModifierSpec, Right(mtid -> Seq(mid)), None)
          val cancellable: Cancellable = context.system.scheduler
            .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mtid, mid))
          peerInfo._2._1.cancel()
          cancellables = cancellables.updated(midAsKey, peer._1 -> (cancellable, peerInfo._2._2 + 1))
        }
      } else {
        cancellables -= midAsKey
        //logInfo(s"Look's like we should delete id ${Algos.encode(mid)} from cancellables")
        val peersToRequest: Seq[ConnectedPeer] = peers.getOrElse(midAsKey, statusTracker.statuses.keys.toSeq)
        //logInfo(s"peersToRequest: ${peersToRequest.map(_.toString)}")
        peersToRequest.headOption.foreach { nextPeer =>
          //logInfo(s"nextPeer: $nextPeer")
          peers = peers.updated(midAsKey, peersToRequest.filter(_ != nextPeer))
//          logInfo(s"Update peers. Now: ${peers.map(modifier => s"Can download ${Algos.encode(modifier._1.toArray)} " +
//            s"from ${modifier._2.map(_.toString).mkString(",")}")}")
          expect(nextPeer, mtid, Seq(mid))
        }
      }
    }
  }

  def isExpecting(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Boolean =
    cancellables.get(key(mid)).exists(_._1 == cp)

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
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] =
      Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    if (settings.influxDB.isDefined)
      context.actorSelection("/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    logInfo(s"statusTracker.statuses: ${statusTracker.statuses.keys.mkString(",")}")
    statusTracker.statuses.keys.foreach(peer => {
      logInfo(s"Expect ${modifierIds.map(Algos.encode).mkString(",")} from $peer")
      expect(peer, modifierTypeId, modifierIds)
    })
  }

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = tryWithLogging {
    logInfo(s"Receive: ${Algos.encode(mid)}")
    if (isExpecting(mtid, mid, cp)) {
      logInfo(s"Add to delivered: ${Algos.encode(mid)}")
      delivered = delivered.updated(key(mid), cp)
      cancellables.get(key(mid)).foreach(_._2._1.cancel())
      cancellables -= key(mid)
      peers -= key(mid)
    }
    else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)
  }
}

object DeliveryManager {

  case class CleanDelivered(modifiersInCache: Seq[mutable.WrappedArray[Byte]])

  case object FullBlockChainSynced

  case object StopSync

  case object ContinueSync

}