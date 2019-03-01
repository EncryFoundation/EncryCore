package encry.network

import java.net.InetAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{HistoryComparisonResult, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history.Header
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManager._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo}
import encry.view.mempool.Mempool
import org.encryfoundation.common.Algos
import encry.settings.EncryAppSettings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable
import scala.util.Random
import BasicMessagesRepo._

class DeliveryManager(influxRef: Option[ActorRef],
                      nodeViewHolderRef: ActorRef,
                      networkControllerRef: ActorRef,
                      system: ActorSystem,
                      settings: EncryAppSettings) extends Actor with StrictLogging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var delivered: HashSet[ModifierIdAsKey] = HashSet.empty[ModifierIdAsKey]
  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var deliveredModifiersMap: Map[ModifierIdAsKey, Seq[InetAddress]] = Map.empty
  var cancellables: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
  var requestedModifiers: Map[ConnectedPeer, HashMap[ModifierIdAsKey, Int]] = Map.empty
  var mempoolReaderOpt: Option[Mempool] = None
  var historyReaderOpt: Option[EncryHistory] = None
  var isBlockChainSynced: Boolean = false
  var isMining: Boolean = settings.node.mining
  //todo check context
  val syncTracker: SyncTracker = SyncTracker(self, context, settings.network)

  def key(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  override def preStart(): Unit = {
    val messageSpecs: Seq[Byte] = Seq(NetworkMessagesIds.Modifier)
    networkControllerRef ! RegisterMessagesHandler(messageSpecs, self)
    syncTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(
      settings.network.modifierDeliverTimeCheck,
      settings.network.modifierDeliverTimeCheck
    )(self ! CheckModifiersToDownload)

    system.scheduler.schedule(
      settings.network.updatePriorityTime.seconds,
      settings.network.updatePriorityTime.seconds
    )(syncTracker.updatePeersPriorityStatus())
  }

  override def receive: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      syncTracker.updateStatus(remote, status)
      status match {
        case Unknown => logger.info("Peer status is still unknown")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => syncTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => syncTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (delivered.contains(key(modifierId))) delivered -= key(modifierId)
      else reexpect(peer, modifierTypeId, modifierId)
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        val currentQueue: HashSet[ModifierId] =
          requestedModifiers.flatMap { case (_, modIds) =>
            modIds.keys.map(modId => ModifierId @@ modId.toArray)
          }.to[HashSet]
        val newIds: Seq[(ModifierTypeId, ModifierId)] =
          h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
            .filterNot(modId => currentQueue.contains(modId._2))
        if (newIds.nonEmpty) newIds.groupBy(_._1).foreach {
          case (modId: ModifierTypeId, ids: Seq[(ModifierTypeId, ModifierId)]) => requestDownload(modId, ids.map(_._2))
        }
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) expect(peer, modifierTypeId, modifierIds)
    case DataFromPeer(message, remote) => message match {
      case ModifiersNetworkMessage(data) =>
        val typeId: ModifierTypeId = data._1
        val modifiers: Map[ModifierId, Array[Byte]] = data._2
        val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
          modifiers partition { case (id, _) => isSpam(id) }
        influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
        for ((id, _) <- modifiers) receive(typeId, id, remote)
        if (spam.nonEmpty) {
          logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
            s": ${spam.keys.map(Algos.encode)}")
          deleteSpam(spam.keys.toSeq)
        }
        val filteredModifiers: Seq[Array[Byte]] = fm.filterNot { case (modId, _) =>
          historyReaderOpt.contains(modId)
        }.values.toSeq
        if (filteredModifiers.nonEmpty) nodeViewHolderRef ! ModifiersFromRemote(typeId, filteredModifiers)
        historyReaderOpt.foreach { h =>
          if (!h.isHeadersChainSynced && cancellables.isEmpty) sendSync(h.syncInfo)
          else if (h.isHeadersChainSynced && !h.isFullChainSynced && cancellables.isEmpty) self ! CheckModifiersToDownload
        }
      case _ => logger.info(s"DeliveryManager got invalid type of DataFromPeer message!")
    }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: ModifierId, previousModifier: Option[ModifierId]) =>
      if (previousModifier.isDefined && isBlockChainSynced)
        priorityRequest(modifierTypeId, Seq(modifiersId), previousModifier.get)
      else requestDownload(modifierTypeId, Seq(modifiersId))
    case FullBlockChainSynced => isBlockChainSynced = true
    case StartMining => isMining = true
    case DisableMining => isMining = false
    case SendLocalSyncInfo =>
      if (syncTracker.elapsedTimeSinceLastSync < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      historyReaderOpt = Some(reader)
    case ChangedMempool(reader: Mempool) if reader.isInstanceOf[Mempool] => mempoolReaderOpt = Some(reader)
    case GetStatusTrackerPeer => sender() ! syncTracker.statuses
  }

  /**
    * If node is not synced, send sync info to random peer, otherwise to all known peers
    *
    * @param syncInfo
    */
  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    if (isBlockChainSynced)
      syncTracker.peersToSyncWith.foreach(peer =>
        peer.handlerRef ! SyncInfoNetworkMessage(syncInfo)
      )
    else Random.shuffle(syncTracker.peersToSyncWith).headOption.foreach(peer =>
      peer.handlerRef ! SyncInfoNetworkMessage(syncInfo)
    )
  }

  //todo: refactor
  def expect(peer: ConnectedPeer, mTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit =
    if (((mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining)
      || mTypeId != Transaction.ModifierTypeId) && syncTracker.statuses.get(peer).exists(_._1 != Younger)) {
      val notYetRequestedIds: Seq[ModifierId] = modifierIds.foldLeft(Vector[ModifierId]()) {
        case (notYetRequested, modId) =>
          if (historyReaderOpt.forall(history => !history.contains(modId) && !delivered.contains(key(modId)))) {
            notYetRequested :+ modId
          } else notYetRequested
      }
      if (notYetRequestedIds.nonEmpty) {
        logger.info(s"Send request to ${peer.socketAddress.getAddress} for modifiers of type $mTypeId " +
          s"with ${modifierIds.size} ids.")
        peer.handlerRef ! RequestModifiersNetworkMessage(settings.network.maxInvObjects, mTypeId -> notYetRequestedIds)
        syncTracker.incrementRequest(peer)
      }
      notYetRequestedIds.foreach { id =>
        val cancellable: Cancellable = context.system.scheduler
          .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, id))
        ///TODO: trouble with several sends of same mod
        val peerMap: Map[ModifierIdAsKey, (Cancellable, Int)] = cancellables
          .getOrElse(peer.socketAddress.getAddress, Map.empty)
          .updated(key(id), cancellable -> 0)
        cancellables = cancellables.updated(peer.socketAddress.getAddress, peerMap)
        val currentPeerModIds: HashMap[ModifierIdAsKey, Int] =
          requestedModifiers.getOrElse(peer, HashMap.empty[ModifierIdAsKey, Int])
        val reqAttempts: Int = currentPeerModIds.getOrElse(key(id), 0) + 1
        requestedModifiers = requestedModifiers.updated(peer, currentPeerModIds.updated(key(id), reqAttempts))
      }
    }

  //todo: refactor
  def reexpect(cp: ConnectedPeer, mTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val peerAndHistoryOpt: Option[(ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus))] =
      syncTracker.statuses.find { case (peer, (comparisonResult, _)) =>
        peer.socketAddress == cp.socketAddress && comparisonResult != Younger
      }
    cancellables.get(cp.socketAddress.getAddress) match {
      case Some(modifiersInfo) =>
        modifiersInfo.get(key(modifierId)) match {
          case Some(modifierInfo) =>
            peerAndHistoryOpt.foreach { peerInfo =>
              if (modifierInfo._2 < settings.network.maxDeliveryChecks &&
                requestedModifiers.get(cp).exists(_.contains(key(modifierId)))) {
                logger.debug(s"Re-ask ${cp.socketAddress} and handler: ${cp.handlerRef} for modifiers of type: " +
                  s"$mTypeId with id: ${Algos.encode(modifierId)}")
                peerInfo._1.handlerRef ! RequestModifiersNetworkMessage(settings.network.maxInvObjects, mTypeId -> Seq(modifierId))
                syncTracker.incrementRequest(cp)
                val cancellable: Cancellable = context.system.scheduler
                  .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mTypeId, modifierId))
                modifierInfo._1.cancel()
                val peerMap = cancellables.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty)
                  .updated(key(modifierId), cancellable -> (modifierInfo._2 + 1))
                cancellables = cancellables.updated(peerInfo._1.socketAddress.getAddress, peerMap)
              } else {
                val peerMap = cancellables.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty) - key(modifierId)
                cancellables = cancellables.updated(peerInfo._1.socketAddress.getAddress, peerMap)

                val peerModIds: HashMap[ModifierIdAsKey, Int] =
                  requestedModifiers.getOrElse(cp, HashMap.empty[ModifierIdAsKey, Int])
                peerModIds.get(key(modifierId)).foreach { qtyOfRequests =>
                  if (qtyOfRequests - 1 == 0) requestedModifiers =
                    requestedModifiers.updated(cp, peerModIds - key(modifierId))
                  else requestedModifiers =
                    requestedModifiers.updated(cp, peerModIds.updated(key(modifierId), qtyOfRequests - 1))
                }
              }
            }
          case None => // Do nothing
        }
      case None => // Do nothing
    }
  }

  def isExpecting(mid: ModifierId, cp: ConnectedPeer): Boolean = requestedModifiers.get(cp).exists(_.contains(key(mid)))

  def deleteSpam(mids: Seq[ModifierId]): Unit = for (id <- mids) deliveredSpam -= key(id)

  def isSpam(mid: ModifierId): Boolean = deliveredSpam contains key(mid)

  def sendExtension(remote: ConnectedPeer, status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit =
    if (isBlockChainSynced) extOpt match {
      case None => logger.info(s"extOpt is empty for: $remote. Its status is: $status.")
      case Some(ext) =>
        ext.groupBy(_._1).mapValues(_.map(_._2)).foreach { case (mid, mods) =>
          networkControllerRef ! SendToNetwork(InvNetworkMessage(settings.network.maxInvObjects, mid -> mods), SendToPeer(remote))
        }
    } else logger.info(s"Peer's $remote history is younger, but node is not synced, so ignore sending extensions")

  def priorityRequest(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId], previousModifier: ModifierId): Unit =
    deliveredModifiersMap.get(key(previousModifier)) match {
      case Some(addresses) if addresses.nonEmpty =>
        syncTracker.statuses.find(_._1.socketAddress.getAddress == addresses.head) match {
          case Some(ph) =>
            deliveredModifiersMap = deliveredModifiersMap - key(previousModifier)
            expect(ph._1, modifierTypeId, modifierIds)
          case None => requestDownload(modifierTypeId, modifierIds)
        }
      case None => requestDownload(modifierTypeId, modifierIds)
    }

  /**
    * If node is not synced, `requestDownload` sends request for the one peer which will be find by 2 criteria:
    * 1) HistoryComparisonResult != Younger.
    * 2) Choose peer with highest priority.
    * Otherwise this function sends requests for all known peers selected by 1-st criterion as above.
    *
    * If there are no any peers, request won't be sent.
    *
    * @param modifierTypeId - requested modifier type id.
    * @param modifierIds    - collection of requested modifiers ids.
    */
  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit =
    if (!isBlockChainSynced) syncTracker.getPeersForConnection.lastOption match {
      case Some((peer, _)) =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        expect(peer, modifierTypeId, modifierIds)
      case None => logger.info(s"BlockChain is not synced. There is no nodes, which we can connect with.")
    }
    else syncTracker.getPeersForConnection match {
      case coll: Vector[_] if coll.nonEmpty =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        coll.foreach { case (peer, _) =>
          logger.debug(s"Sent download request to the ${peer.socketAddress}.")
          expect(peer, modifierTypeId, modifierIds)
        }
      case _ => logger.info(s"BlockChain is synced. There is no nodes, which we can connect with.")
    }

  def receive(mTid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit =
    if (isExpecting(mid, cp)) {
      logger.debug(s"Peer ${cp.socketAddress} got new modifier.")
      syncTracker.incrementReceive(cp)
      //todo: refactor
      delivered = delivered + key(mid)
      val peerMap: Map[ModifierIdAsKey, (Cancellable, PeerPriorityStatus)] =
        cancellables.getOrElse(cp.socketAddress.getAddress, Map.empty)
      peerMap.get(key(mid)).foreach(_._1.cancel())
      val peerModIds: HashMap[ModifierIdAsKey, Int] =
        requestedModifiers.getOrElse(cp, HashMap.empty[ModifierIdAsKey, Int])
      requestedModifiers = requestedModifiers.updated(cp, peerModIds - key(mid))
      val peerMapWithoutModifier: Map[ModifierIdAsKey, (Cancellable, PeerPriorityStatus)] = peerMap - key(mid)
      cancellables = cancellables.updated(cp.socketAddress.getAddress, peerMapWithoutModifier)
      if (isBlockChainSynced && mTid == Header.modifierTypeId) {
        val peersWhoDelivered: Seq[InetAddress] = deliveredModifiersMap
          .getOrElse(key(mid), Seq.empty) :+ cp.socketAddress.getAddress
        deliveredModifiersMap = deliveredModifiersMap.updated(key(mid), peersWhoDelivered)
      }
    } else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)
}

object DeliveryManager {

  case object FullBlockChainSynced

  case object GetStatusTrackerPeer

}