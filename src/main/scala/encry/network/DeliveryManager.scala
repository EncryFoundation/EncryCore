package encry.network

import java.net.InetAddress

import akka.actor.{Actor, Cancellable}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.{networkController, nodeViewHolder, settings}
import encry.consensus.History.{HistoryComparisonResult, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history.Header
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManager.FullBlockChainSynced
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message._
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.Mempool
import org.encryfoundation.common.Algos

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class DeliveryManager extends Actor with StrictLogging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var delivered: HashSet[ModifierIdAsKey] = HashSet.empty[ModifierIdAsKey]
  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var deliveredModifiersMap: Map[ModifierIdAsKey, Seq[InetAddress]] = Map.empty
  var cancellables: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
  var requestedModifiers: Map[ModifierIdAsKey, Int] = Map.empty
  var mempoolReaderOpt: Option[Mempool] = None
  var historyReaderOpt: Option[EncryHistory] = None
  var isBlockChainSynced: Boolean = false
  var isMining: Boolean = settings.node.mining
  val invSpec: InvSpec = new InvSpec(settings.network.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
  val statusTracker: SyncTracker = SyncTracker(self, context, settings.network)

  def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(ModifiersSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(
      settings.network.modifierDeliverTimeCheck,
      settings.network.modifierDeliverTimeCheck
    )(self ! CheckModifiersToDownload)
  }

  override def receive: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => logger.info("Peer status is still unknown")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (delivered.contains(key(modifierId))) delivered -= key(modifierId)
      else reexpect(peer, modifierTypeId, modifierId)
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = requestedModifiers.keys.map(modId => ModifierId @@ modId.toArray)
        val newIds: Seq[(ModifierTypeId, ModifierId)] =
          h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
            .filterNot(modId => requestedModifiers.contains(key(modId._2)))
        if (newIds.nonEmpty) newIds.groupBy(_._1).foreach {
          case (modId: ModifierTypeId, ids: Seq[(ModifierTypeId, ModifierId)]) => requestDownload(modId, ids.map(_._2))
        }
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) expect(peer, modifierTypeId, modifierIds)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => isSpam(id) }
      if (settings.influxDB.isDefined)
        context.actorSelection("/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      if (spam.nonEmpty) {
        logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Algos.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      val filteredModifiers = fm.filterNot{ case (modId, _) => historyReaderOpt.contains(modId)}.values.toSeq
      if (filteredModifiers.nonEmpty)nodeViewHolder ! ModifiersFromRemote(typeId, filteredModifiers)
      historyReaderOpt.foreach { h =>
        if (!h.isHeadersChainSynced && cancellables.isEmpty) sendSync(h.syncInfo)
        else if (h.isHeadersChainSynced && !h.isFullChainSynced && cancellables.isEmpty) self ! CheckModifiersToDownload
      }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: ModifierId, previousModifier: Option[ModifierId]) =>
      if (previousModifier.isDefined && isBlockChainSynced) priorityRequest(modifierTypeId, Seq(modifiersId), previousModifier.get)
      else requestDownload(modifierTypeId, Seq(modifiersId))
    case FullBlockChainSynced => isBlockChainSynced = true
    case StartMining => isMining = true
    case DisableMining => isMining = false
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      historyReaderOpt = Some(reader)
    case ChangedMempool(reader: Mempool) if reader.isInstanceOf[Mempool] => mempoolReaderOpt = Some(reader)
  }

  /**
    * If node is not synced, send sync info to random peer, overwise to all known peers
    * @param syncInfo
    */
  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    if (isBlockChainSynced) statusTracker.peersToSyncWith().foreach(peer =>
      peer.handlerRef ! Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
    ) else Random.shuffle(statusTracker.peersToSyncWith()).headOption.foreach(peer =>
      peer.handlerRef ! Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
    )
  }

  //todo: refactor
  def expect(peer: ConnectedPeer, mTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit =
    if (((mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining)
      || mTypeId != Transaction.ModifierTypeId) && statusTracker.statuses.get(peer).exists(_ != Younger)) {
      val notYetRequestedIds: Seq[ModifierId] = modifierIds.foldLeft(Vector[ModifierId]()) {
        case (notYetRequested, modId) =>
          if (historyReaderOpt.forall(history => !history.contains(modId) && !delivered.contains(key(modId)))) {
            notYetRequested :+ modId
          } else notYetRequested
      }
      if (notYetRequestedIds.nonEmpty) {
        logger.info(s"Send request to ${peer.socketAddress.getAddress} for modifiers of type $mTypeId with ids: " +
          s"${modifierIds.map(Algos.encode).mkString(",")}")
        peer.handlerRef ! Message(requestModifierSpec, Right(mTypeId -> notYetRequestedIds), None)
      }
      notYetRequestedIds.foreach { id =>
        val cancellable: Cancellable = context.system.scheduler
          .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, id))
        val peerMap = cancellables.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .updated(key(id), cancellable -> 0)
        cancellables = cancellables.updated(peer.socketAddress.getAddress, peerMap)
        val reqAttempts = requestedModifiers.getOrElse(key(id), 0) + 1
        requestedModifiers = requestedModifiers.updated(key(id), reqAttempts)
      }
    }

  //todo: refactor
  def reexpect(cp: ConnectedPeer, mTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val peerAndHistoryOpt: Option[(ConnectedPeer, HistoryComparisonResult)] =
      statusTracker.statuses.find(peer => peer._1.socketAddress == cp.socketAddress && peer._2 != Younger)
    cancellables.get(cp.socketAddress.getAddress) match {
      case Some(modifiersInfo) =>
        modifiersInfo.get(key(modifierId)) match {
          case Some(modifierInfo)=>
            peerAndHistoryOpt.foreach { peerInfo =>
              if (modifierInfo._2 < settings.network.maxDeliveryChecks && requestedModifiers.contains(key(modifierId))) {
                logger.debug(s"Re-ask ${cp.socketAddress} and handler: ${cp.handlerRef} for modifiers of type: " +
                  s"$mTypeId with id: ${Algos.encode(modifierId)}")
                peerInfo._1.handlerRef ! Message(requestModifierSpec, Right(mTypeId -> Seq(modifierId)), None)
                val cancellable: Cancellable = context.system.scheduler
                  .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mTypeId, modifierId))
                modifierInfo._1.cancel()
                val peerMap = cancellables.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty)
                  .updated(key(modifierId), cancellable -> (modifierInfo._2 + 1))
                cancellables = cancellables.updated(peerInfo._1.socketAddress.getAddress, peerMap)
              } else {
                val peerMap = {
                  cancellables.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty) - key(modifierId)
                }
                cancellables = cancellables.updated(peerInfo._1.socketAddress.getAddress, peerMap)
                requestedModifiers.get(key(modifierId)).foreach{qtyOfRequests =>
                  if (qtyOfRequests - 1 == 0) requestedModifiers = requestedModifiers - key(modifierId)
                  else requestedModifiers = requestedModifiers.updated(key(modifierId), qtyOfRequests - 1)
                }
              }
            }
          case None => // Do nothing
        }
      case None => // Do nothing
    }
  }

  def isExpecting(mtid: ModifierTypeId, mid: ModifierId): Boolean = requestedModifiers.keys.toList.contains(key(mid))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) deliveredSpam -= key(id)

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def sendExtension(remote: ConnectedPeer, status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit =
    if (isBlockChainSynced) extOpt match {
      case None => logger.info(s"extOpt is empty for: $remote. Its status is: $status.")
      case Some(ext) =>
        ext.groupBy(_._1).mapValues(_.map(_._2)).foreach { case (mid, mods) =>
          networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
        }
     }
    else logger.info(s"Peer's $remote hisotry is younger, but node is note synces, so ignore sending extentions")

  def priorityRequest(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId], previousModifier: ModifierId): Unit =
    deliveredModifiersMap.get(key(previousModifier)) match {
      case Some(addresses) if addresses.nonEmpty =>
        statusTracker.statuses.find(_._1.socketAddress.getAddress == addresses.head) match {
          case Some(ph) =>
            deliveredModifiersMap = deliveredModifiersMap - key(previousModifier)
            expect(ph._1, modifierTypeId, modifierIds)
          case None => requestDownload(modifierTypeId, modifierIds)
        }
      case None => requestDownload(modifierTypeId, modifierIds)
    }

  /**
    * If node is not synced, send request to random peer with non-younger history, overwise to all peers with
    * non-younger history
    * @param modifierTypeId
    * @param modifierIds
    */
  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    if (settings.influxDB.isDefined)
      context.actorSelection("/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    if (!isBlockChainSynced)
      Random.shuffle(statusTracker.statuses.filter(_._2 != Younger)).headOption.foreach(pI =>
        expect(pI._1, modifierTypeId, modifierIds)
      )
    else
      statusTracker.statuses.filter(_._2 != Younger).keys.foreach(expect(_, modifierTypeId, modifierIds))
  }

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = if (isExpecting(mtid, mid)) {
    //todo: refactor
    delivered = delivered + key(mid)
    val peerMap = cancellables.getOrElse(cp.socketAddress.getAddress, Map.empty)
    peerMap.get(key(mid)).foreach(_._1.cancel())
    requestedModifiers = requestedModifiers - key(mid)
    val peerMapWithoutModifier = peerMap - key(mid)
    cancellables = cancellables.updated(cp.socketAddress.getAddress, peerMapWithoutModifier)
    if (isBlockChainSynced && mtid == Header.modifierTypeId) {
      val peersWhoDelivered = deliveredModifiersMap.getOrElse(key(mid), Seq.empty) :+ cp.socketAddress.getAddress
      deliveredModifiersMap = deliveredModifiersMap.updated(key(mid), peersWhoDelivered)
    }
  }
  else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)

}

object DeliveryManager {

  case object FullBlockChainSynced
}