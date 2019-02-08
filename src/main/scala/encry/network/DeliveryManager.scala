package encry.network

import java.net.InetAddress
import akka.actor.{Actor, Cancellable}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.{networkController, nodeViewHolder, settings}
import encry.consensus.History.{Fork, HistoryComparisonResult, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history.Header
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManager.{DeleteModIdFromDelivaeryMap, FullBlockChainSynced}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message.{InvSpec, Message, ModifiersSpec, RequestModifierSpec}
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.Mempool
import org.encryfoundation.common.Algos
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class DeliveryManager extends Actor with StrictLogging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var delivered: Map[ModifierIdAsKey, Int] = Map.empty[ModifierIdAsKey, Int]
  var deliveredModifiersMap: Map[ModifierIdAsKey, Set[InetAddress]] =
    Map.empty[ModifierIdAsKey, Set[InetAddress]]
  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var cancellables: Map[InetAddress, Map[ModifierId, (Cancellable, Int)]] = Map.empty
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
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler
      .schedule(settings.network.syncInterval, settings.network.syncInterval)(self ! CheckModifiersToDownload)
  }

  override def receive: Receive = syncSending orElse netMessages

  def syncSending: Receive = {
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else {
        logger.info(s"Trying to send local sync. historyReaderOpt: ${historyReaderOpt}")
        historyReaderOpt.foreach(r => sendSync(r.syncInfo))
      }
  }

  def netMessages: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => logger.info("Peer status is still unknown")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case DeleteModIdFromDelivaeryMap(id: ModifierId) =>
      deliveredModifiersMap = deliveredModifiersMap - key(id)
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      logger.info(s"Going to check deliver of modifier: ${Algos.encode(modifierId)} with type ${modifierTypeId}")
      if (delivered.get(key(modifierId)).contains(1)) {
        logger.info(s"Modifier with id: ${Algos.encode(modifierId)} delivered and num is ${delivered.get(key(modifierId))}!")
        delivered -= key(modifierId)
      } else if (delivered.contains(key(modifierId))) {
        logger.info(s"Modifier with id: ${Algos.encode(modifierId)} delivered and num is ${delivered.get(key(modifierId))}!")
        val prevNum = delivered(key(modifierId))
        delivered = delivered.updated(key(modifierId), prevNum - 1)
      }
      else {
        logger.info(s"Modifier with id: ${Algos.encode(modifierId)} not delivered from ${peer.socketAddress.getAddress}! Reexpect!")
        reexpect(peer, modifierTypeId, modifierId)
      }
    case CheckModifiersToDownload =>
      logger.info("CheckModifiersToDownload!")
      logger.info(s"historyReaderOpt: ${historyReaderOpt}")
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = requestedModifiers.keys.map(mod => ModifierId @@ mod.toArray)
        logger.info(s"currentQueue: ${currentQueue.map(Algos.encode).mkString(",")}")
        val newIds: Seq[(ModifierTypeId, ModifierId)] =
          h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
            .filterNot(modId => requestedModifiers.contains(key(modId._2)))
        logger.info(s"++++++newIds: ${newIds.map{case (typeMod, id) => s"($typeMod, ${Algos.encode(id)})"}.mkString(",")}")
        newIds.groupBy(_._1).foreach {
          case (modId: ModifierTypeId, ids: Seq[(ModifierTypeId, ModifierId)]) => {
            logger.info(s"Request from delivery manager of type $modId with ids: ${ids.map(id => Algos.encode(id._2)).mkString(",")}")
            requestDownload(modId, ids.map(_._2))
          }
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
      fm.foreach{modifierSer =>
        if (data._1 == Header.modifierTypeId) {
          val newPeersMap =
            deliveredModifiersMap.getOrElse(key(modifierSer._1), Set.empty[InetAddress]) + remote.socketAddress.getAddress
          deliveredModifiersMap = deliveredModifiersMap.updated(key(modifierSer._1), newPeersMap)
          logger.info(s"Update deliveredModifiersMap for key ${Algos.encode(modifierSer._1)} now:" +
            s" ${deliveredModifiersMap.getOrElse(key(modifierSer._1), Seq.empty[InetAddress])}")
        }
        if (delivered.get(key(modifierSer._1)).contains(1))
          nodeViewHolder ! ModifiersFromRemote(typeId, Seq(modifierSer._2))
      }
      historyReaderOpt.foreach { h =>
        if (!h.isHeadersChainSynced && cancellables.isEmpty) sendSync(h.syncInfo)
        else if (h.isHeadersChainSynced && !h.isFullChainSynced) self ! CheckModifiersToDownload
      }
    case DownloadRequest(modifierTypeId: ModifierTypeId,
                         modifierId: ModifierId,
                         previousModifier: Option[ModifierId]) if previousModifier.isDefined =>
      priorityRequest(modifierTypeId, Seq(modifierId), previousModifier.get)
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId, previousModifier: Option[ModifierId]) =>
      requestDownload(modifierTypeId, Seq(modifierId))
    case FullBlockChainSynced => isBlockChainSynced = true
    case StartMining => isMining = true
    case DisableMining => isMining = false
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else {
        logger.info(s"Trying to send local sync. historyReaderOpt: ${historyReaderOpt}")
        historyReaderOpt.foreach(r => sendSync(r.syncInfo))
      }
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      if (historyReaderOpt.isEmpty) self ! SendLocalSyncInfo
      historyReaderOpt = Some(reader)
    case ChangedMempool(reader: Mempool) if reader.isInstanceOf[Mempool] => mempoolReaderOpt = Some(reader)
  }

  /**
    * Send msg Sync to knownPeers. If node is synced: Send syncinfo to all knownpeer else to random
    * @param syncInfo
    */
  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    if (isBlockChainSynced) statusTracker.peersToSyncWith().foreach(peer =>
      peer.handlerRef ! Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
    ) else {
      val peerToSync = statusTracker.statuses.filter(_._2 != Younger).keys.toSeq
      if (peerToSync.nonEmpty)
        peerToSync(Math.abs(Random.nextInt(peerToSync.length))).handlerRef !
          Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
    }
  }

  //todo: refactor
  def expect(peer: ConnectedPeer, mTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit =
    if (((mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining)
      || mTypeId != Transaction.ModifierTypeId) && statusTracker.statuses.get(peer).exists(_ != Younger)) {
      val notYetRequestedIds: Seq[ModifierId] = modifierIds.foldLeft(Vector[ModifierId]()) {
        case (notYetRequested, modId) =>
          if (historyReaderOpt.forall(history => !history.contains(modId) && !delivered.contains(key(modId))) &&
            cancellables.get(peer.socketAddress.getAddress).forall(peerMap => !peerMap.contains(modId))
          ) {
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
          .updated(id, cancellable -> 0)
        cancellables = cancellables.updated(peer.socketAddress.getAddress, peerMap)
        logger.info(s"before update requestedModifiers.get(${Algos.encode(id)}) = ${requestedModifiers.get(key(id))}")
        requestedModifiers = requestedModifiers.updated(key(id), requestedModifiers.getOrElse(key(id), 0) + 1)
        logger.info(s"after update requestedModifiers.get(${Algos.encode(id)}) = ${requestedModifiers.get(key(id))}")
      }
    }

  //todo: refactor
  def reexpect(cp: ConnectedPeer, mTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    logger.info(s"Trying to reexpect ${Algos.encode(modifierId)} from ${cp.socketAddress.getAddress}")
    cancellables.get(cp.socketAddress.getAddress) match {
      case Some(modifiersInfo) =>
        modifiersInfo.get(modifierId) match {
          case Some(modifierInfo) =>
              if (modifierInfo._2 < settings.network.maxDeliveryChecks && requestedModifiers.contains(key(modifierId)) &&
                historyReaderOpt.forall(history => !history.contains(modifierId))) {
                logger.info(s"Re-ask ${cp.socketAddress} and handler: ${cp.handlerRef} for modifiers of type: " +
                  s"$mTypeId with id: ${Algos.encode(modifierId)}")
                cp.handlerRef ! Message(requestModifierSpec, Right(mTypeId -> Seq(modifierId)), None)
                val cancellable: Cancellable = context.system.scheduler
                  .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mTypeId, modifierId))
                modifierInfo._1.cancel()
                val peerMap = cancellables.getOrElse(cp.socketAddress.getAddress, Map.empty)
                  .updated(modifierId, cancellable -> (modifierInfo._2 + 1))
                cancellables = cancellables.updated(cp.socketAddress.getAddress, peerMap)
              } else {
                val peerMap = {
                  cancellables.getOrElse(cp.socketAddress.getAddress, Map.empty) - modifierId
                }
                cancellables = cancellables.updated(cp.socketAddress.getAddress, peerMap)
                requestedModifiers.get(key(modifierId)).foreach{qtyOfRequests =>
                  if (qtyOfRequests - 1 == 0) {
                    requestedModifiers = requestedModifiers - key(modifierId)
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
    else logger.info(s"Peer's $remote history is younger, but node is note synces, so ignore sending extentions")

  def priorityRequest(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId], previousModifier: ModifierId): Unit = {
    logger.info("Trying to get priority request")
    deliveredModifiersMap.get(key(previousModifier)) match {
      case Some(addresses) if addresses.nonEmpty =>
        logger.info(s"Prev sender exists: ${addresses}")
        statusTracker.statuses.find(_._1.socketAddress.getAddress == addresses.head) match {
          case Some(ph) =>
            logger.info("Handler exists!")
            deliveredModifiersMap = deliveredModifiersMap - key(previousModifier)
            expect(ph._1, modifierTypeId, modifierIds)
          case None => requestDownload(modifierTypeId, modifierIds)
        }
      case None => requestDownload(modifierTypeId, modifierIds)
    }
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    if (settings.influxDB.isDefined)
      context.actorSelection("/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    statusTracker.statuses.filter{case (peer, cHR) => cHR != Younger && cHR != Fork}.keys.foreach(expect(_, modifierTypeId, modifierIds))
  }

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit = if (isExpecting(mtid, mid)) {
    //todo: refactor
    logger.debug(s"Get modifier of id: ${Algos.encode(mid)} from ${cp.socketAddress.getAddress}")
    delivered = delivered.updated(key(mid), delivered.getOrElse(key(mid), 0) + 1)
    val peerMap = cancellables.getOrElse(cp.socketAddress.getAddress, Map.empty)
    peerMap.get(mid).foreach(_._1.cancel())
    val peerMapWithoutModifier = peerMap - mid
    requestedModifiers = requestedModifiers.updated(key(mid), requestedModifiers.getOrElse(key(mid), 1) - 1)
    if (requestedModifiers.get(key(mid)).contains(0)) requestedModifiers = requestedModifiers - key(mid)
    cancellables = cancellables.updated(cp.socketAddress.getAddress, peerMapWithoutModifier)
  }
  else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)

}

object DeliveryManager {

  case object FullBlockChainSynced

  case class DeleteModIdFromDelivaeryMap(id: ModifierId)
}