package encry.network

import java.net.InetAddress
import akka.actor.{Actor, ActorRef, Cancellable, Stash}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{HistoryComparisonResult, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
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
import org.encryfoundation.common.Algos
import encry.settings.EncryAppSettings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Random
import BasicMessagesRepo._

class DeliveryManager(influxRef: Option[ActorRef],
                      nodeViewHolderRef: ActorRef,
                      networkControllerRef: ActorRef,
                      settings: EncryAppSettings) extends Actor with StrictLogging with Stash {

  //TODO try to implement local dispatcher and create implicit val executionContext = context.dispatcher
  //TODO try to implement mailbox with priorities ( the best priority wil be with UpdateHistory message )

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  var deliveredSpam: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  var receivedModifiers: Map[InetAddress, Set[ModifierIdAsKey]] = Map.empty
  var expectedModifiers: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
  val syncTracker: SyncTracker = SyncTracker(self, context, settings.network)

  override def preStart(): Unit = {
    networkControllerRef ! RegisterMessagesHandler(Seq(ModifiersNetworkMessage.NetworkMessageTypeID), self)
    context.system.eventStream.subscribe(self, classOf[HistoryChanges])
  }

  override def receive: Receive = {
    case HistoryChanges(historyReader) =>
      unstashAll()
      logger.info(s"Got message with history. All messages will be unstashed. Starting all schedulers.")
      context.system.scheduler.schedule(5.second, settings.network.modifierDeliverTimeCheck)(self ! CheckModifiersToDownload)
      context.system.scheduler.schedule(5.second, settings.network.updatePriorityTime.seconds)(syncTracker.updatePeersPriorityStatus())
      syncTracker.scheduleSendSyncInfo()
      context.become(basicMessageHandler(historyReader, isBlockChainSynced = false, isMining = false))
    case message =>
      logger.info(s"Got new message $message while awaiting history. This message will be stashed.")
      stash()
  }

  def basicMessageHandler(history: EncryHistory, isBlockChainSynced: Boolean, isMining: Boolean): Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      syncTracker.updateStatus(remote, status)
      status match {
        case Unknown => logger.info("Peer status is still unknown.")
        case Younger => sendExtension(remote, status, extOpt, isBlockChainSynced)
        case _ =>
      }
    case HandshakedPeer(remote) => syncTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => syncTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      val modIdsFromPeer: Set[ModifierIdAsKey] = receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
      modIdsFromPeer.find(id => id == toKey(modifierId)) match {
        case Some(_) if modIdsFromPeer.size <= 1 => receivedModifiers = receivedModifiers - peer.socketAddress.getAddress
        case Some(_) => receivedModifiers =
          receivedModifiers.updated(peer.socketAddress.getAddress, modIdsFromPeer - toKey(modifierId))
        case None => reRequestModifier(peer, modifierTypeId, modifierId)
      }
    case CheckModifiersToDownload =>
      val requestedMods: HashSet[ModifierId] = expectedModifiers.flatMap { case (_, modIds) =>
        modIds.keys.map(modId => ModifierId @@ modId.toArray)
      }.to[HashSet]
      history.modifiersToDownload(settings.network.networkChunkSize - requestedMods.size, requestedMods)
        .groupBy(_._1)
        .foreach { case (modId, ids) =>
          requestDownload(modId, ids.map(_._2), history, isBlockChainSynced, isMining)
        }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
    case DataFromPeer(message, remote) => message match {
      case ModifiersNetworkMessage((typeId, modifiers)) =>
        val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
          modifiers.partition { case (id, _) => isSpam(id) }
        influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
        for ((id, _) <- modifiers) receive(typeId, id, remote, isBlockChainSynced)
        if (spam.nonEmpty) {
          logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
            s": ${spam.keys.map(Algos.encode)}")
          deleteSpam(spam.keys.toSeq)
        }
        val filteredModifiers: Seq[Array[Byte]] = fm.filterNot { case (modId, _) => history.contains(modId) }.values.toSeq
        if (filteredModifiers.nonEmpty) nodeViewHolderRef ! ModifiersFromRemote(typeId, filteredModifiers)
        if (!history.isHeadersChainSynced && expectedModifiers.isEmpty) sendSync(history.syncInfo, isBlockChainSynced)
        else if (history.isHeadersChainSynced && !history.isFullChainSynced && expectedModifiers.isEmpty) self ! CheckModifiersToDownload
      case _ => logger.info(s"DeliveryManager got invalid type of DataFromPeer message!")
    }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: ModifierId, previousModifier: Option[ModifierId]) =>
      if (previousModifier.isDefined && isBlockChainSynced)
        priorityRequest(modifierTypeId, Seq(modifiersId), previousModifier.get, history, isBlockChainSynced, isMining)
      else requestDownload(modifierTypeId, Seq(modifiersId), history, isBlockChainSynced, isMining)
    case SendLocalSyncInfo =>
      if (syncTracker.elapsedTimeSinceLastSync < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else sendSync(history.syncInfo, isBlockChainSynced)
    case GetStatusTrackerPeer => sender() ! syncTracker.statuses
    case FullBlockChainSynced => context.become(basicMessageHandler(history, isBlockChainSynced = true, isMining))
    case StartMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = true))
    case DisableMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = false))
    case HistoryChanges(historyReader) => context.become(basicMessageHandler(historyReader, isBlockChainSynced, isMining))
    case message => logger.info(s"Got strange message $message on DeliveryManager.")
  }

  /**
    * If node is not synced, send sync info to random peer, otherwise to all known peers.
    *
    * @param syncInfo
    */
  //TODO remove random shuffle and refactor syncTracker
  def sendSync(syncInfo: EncrySyncInfo, isBlockChainSynced: Boolean): Unit =
    if (isBlockChainSynced) syncTracker.peersToSyncWith.foreach(peer => peer.handlerRef ! SyncInfoNetworkMessage(syncInfo))
    else Random.shuffle(syncTracker.peersToSyncWith).headOption.foreach(peer => peer.handlerRef ! SyncInfoNetworkMessage(syncInfo))

  /**
    *
    * @param history
    * @param peer
    * @param mTypeId
    * @param modifierIds
    * @param isBlockChainSynced
    * @param isMining
    */

  def requestModifies(history: EncryHistory,
                      peer: ConnectedPeer,
                      mTypeId: ModifierTypeId,
                      modifierIds: Seq[ModifierId],
                      isBlockChainSynced: Boolean,
                      isMining: Boolean): Unit = {

    val firstCondition: Boolean = mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining
    val secondCondition: Boolean = mTypeId != Transaction.ModifierTypeId
    val thirdCondition: Boolean = syncTracker.statuses.get(peer).exists(_._1 != Younger)

    if ((firstCondition || secondCondition) && thirdCondition) {
      val requestedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, PeerPriorityStatus)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)

      val notYetRequested: Seq[ModifierId] = modifierIds.filter(id =>
        !history.contains(id) && !receivedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .forall(mod => mod != toKey(id)) && !requestedModifiersFromPeer.contains(toKey(id))
      )

      if (notYetRequested.nonEmpty) {
        logger.info(s"Send request to ${peer.socketAddress.getAddress} for modifiers of type $mTypeId " +
          s"with ${modifierIds.size} ids.")
        peer.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> notYetRequested)
        syncTracker.incrementRequest(peer)

        val requestedModIds = notYetRequested.foldLeft(requestedModifiersFromPeer) { case (rYet, id) =>
          rYet.updated(toKey(id), context.system
            .scheduler.scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, id)) -> 1)
        }
        expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, requestedModIds)
      }
    }
  }

  //todo: refactor
  def expect(history: EncryHistory,
             peer: ConnectedPeer,
             mTypeId: ModifierTypeId,
             modifierIds: Seq[ModifierId],
             isBlockChainSynced: Boolean,
             isMining: Boolean): Unit =
    if (((mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining)
      || mTypeId != Transaction.ModifierTypeId) && syncTracker.statuses.get(peer).exists(_._1 != Younger)) {
      val notYetRequestedIds: Seq[ModifierId] = modifierIds.foldLeft(Vector[ModifierId]()) {
        case (notYetRequested, modId) =>
          if (!history.contains(modId)
          //!delivered.contains(toKey(modId))
          ) {
            notYetRequested :+ modId
          } else notYetRequested
      }
      if (notYetRequestedIds.nonEmpty) {
        logger.info(s"Send request to ${peer.socketAddress.getAddress} for modifiers of type $mTypeId " +
          s"with ${modifierIds.size} ids.")
        peer.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> notYetRequestedIds)
        syncTracker.incrementRequest(peer)
      }
      notYetRequestedIds.foreach { id =>
        val cancellable: Cancellable = context.system.scheduler
          .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, id))
        ///TODO: trouble with several sends of same mod
        val peerMap: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
          .getOrElse(peer.socketAddress.getAddress, Map.empty)
          .updated(toKey(id), cancellable -> 0)
        expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerMap)
        //        val currentPeerModIds: HashMap[ModifierIdAsKey, Int] =
        //          requestedModifiers.getOrElse(peer, HashMap.empty[ModifierIdAsKey, Int])
        //        val reqAttempts: Int = currentPeerModIds.getOrElse(toKey(id), 0) + 1
        //        requestedModifiers = requestedModifiers.updated(peer, currentPeerModIds.updated(toKey(id), reqAttempts))
      }
    }

  /**
    *
    * @param peer
    * @param mTypeId
    * @param modId
    */

  def reRequestModifier(peer: ConnectedPeer, mTypeId: ModifierTypeId, modId: ModifierId): Unit = {
    val peerRequests: Map[ModifierIdAsKey, (Cancellable, Int)] =
      expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
    peerRequests.get(toKey(modId)) match {
      case Some((timer, attempts)) =>
        syncTracker.statuses.find { case (innerPeer, (cResult, _)) =>
          innerPeer.socketAddress == peer.socketAddress && cResult != Younger
        }.foreach {
          case (localPeer, _) if attempts < settings.network.maxDeliveryChecks && peerRequests.contains(toKey(modId)) =>
            localPeer.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> Seq(modId))
            logger.debug(s"Re-asked ${peer.socketAddress} and handler: ${peer.handlerRef} for modifier of type: " +
              s"$mTypeId with id: ${Algos.encode(modId)}")
            syncTracker.incrementRequest(peer)
            timer.cancel()
            expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerRequests.updated(toKey(modId),
              context.system.scheduler
                .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, modId)) -> (attempts + 1)
            ))
          case _ => expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerRequests - toKey(modId))
        }
      case None => logger.info(s"Tried to re-ask modifier ${Algos.encode(modId)}, but this id not needed from this peer")
    }
  }

  //todo: refactor
  def reexpect(cp: ConnectedPeer, mTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val peerAndHistoryOpt: Option[(ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus))] =
      syncTracker.statuses.find { case (peer, (comparisonResult, _)) =>
        peer.socketAddress == cp.socketAddress && comparisonResult != Younger
      }
    expectedModifiers.get(cp.socketAddress.getAddress) match {
      case Some(modifiersInfo) =>
        modifiersInfo.get(toKey(modifierId)) match {
          case Some(modifierInfo) =>
            peerAndHistoryOpt.foreach { peerInfo =>
              //              if (modifierInfo._2 < settings.network.maxDeliveryChecks &&
              //                requestedModifiers.get(cp).exists(_.contains(toKey(modifierId)))) {
              //                logger.debug(s"Re-ask ${cp.socketAddress} and handler: ${cp.handlerRef} for modifiers of type: " +
              //                  s"$mTypeId with id: ${Algos.encode(modifierId)}")
              //                peerInfo._1.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> Seq(modifierId))
              //                syncTracker.incrementRequest(cp)
              //                val cancellable: Cancellable = context.system.scheduler
              //                  .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(cp, mTypeId, modifierId))
              //                modifierInfo._1.cancel()
              //                val peerMap = expectedModifiers.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty)
              //                  .updated(toKey(modifierId), cancellable -> (modifierInfo._2 + 1))
              //                expectedModifiers = expectedModifiers.updated(peerInfo._1.socketAddress.getAddress, peerMap)
              //              } else {
              //                val peerMap = expectedModifiers.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty) - toKey(modifierId)
              //                expectedModifiers = expectedModifiers.updated(peerInfo._1.socketAddress.getAddress, peerMap)
              //
              //                val peerModIds: HashMap[ModifierIdAsKey, Int] =
              //                  requestedModifiers.getOrElse(cp, HashMap.empty[ModifierIdAsKey, Int])
              //                peerModIds.get(toKey(modifierId)).foreach { qtyOfRequests =>
              //                  if (qtyOfRequests - 1 == 0) requestedModifiers =
              //                    requestedModifiers.updated(cp, peerModIds - toKey(modifierId))
              //                  else requestedModifiers =
              //                    requestedModifiers.updated(cp, peerModIds.updated(toKey(modifierId), qtyOfRequests - 1))
              //                }
              //              }
            }
          case None => // Do nothing
        }
      case None => // Do nothing
    }
  }

  /**
    *
    * @param mId
    * @param peer
    * @return
    */
  def isExpecting(mId: ModifierId, peer: ConnectedPeer): Boolean =
    expectedModifiers.get(peer.socketAddress.getAddress).exists(_.contains(toKey(mId)))

  /**
    *
    * @param mIds
    */
  def deleteSpam(mIds: Seq[ModifierId]): Unit = for (id <- mIds) deliveredSpam -= toKey(id)

  /**
    *
    * @param mId
    * @return
    */
  def isSpam(mId: ModifierId): Boolean = deliveredSpam.contains(toKey(mId))

  /**
    *
    * @param remote
    * @param status
    * @param extOpt
    * @param isBlockChainSynced
    */
  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]],
                    isBlockChainSynced: Boolean): Unit =
    if (isBlockChainSynced) extOpt match {
      case None => logger.info(s"extOpt is empty for: $remote. Its status is: $status.")
      case Some(ext) =>
        //TODO CHECK SIZE
        ext.groupBy(_._1).mapValues(_.map(_._2)).foreach { case (mid, mods) =>
          networkControllerRef ! SendToNetwork(InvNetworkMessage(mid -> mods), SendToPeer(remote))
        }
    } else logger.info(s"Peer's $remote history is younger, but node is not synced, so ignore sending extensions")

  /**
    *
    * @param modifierTypeId
    * @param modifierIds
    * @param previousModifier
    * @param history
    * @param isBlockChainSynced
    * @param isMining
    */
  def priorityRequest(modifierTypeId: ModifierTypeId,
                      modifierIds: Seq[ModifierId],
                      previousModifier: ModifierId,
                      history: EncryHistory,
                      isBlockChainSynced: Boolean,
                      isMining: Boolean): Unit = deliveredModifiersMap.get(toKey(previousModifier)) match {
    case Some(addresses) if addresses.nonEmpty =>
      syncTracker.statuses.find(_._1.socketAddress.getAddress == addresses.head) match {
        case Some(ph) =>
          deliveredModifiersMap = deliveredModifiersMap - toKey(previousModifier)
          requestModifies(history, ph._1, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
        case None => requestDownload(modifierTypeId, modifierIds, history, isBlockChainSynced, isMining)
      }
    case None => requestDownload(modifierTypeId, modifierIds, history, isBlockChainSynced, isMining)
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
  /**
    *
    * @param modifierTypeId
    * @param modifierIds
    * @param history
    * @param isBlockChainSynced
    * @param isMining
    */
  def requestDownload(modifierTypeId: ModifierTypeId,
                      modifierIds: Seq[ModifierId],
                      history: EncryHistory,
                      isBlockChainSynced: Boolean,
                      isMining: Boolean): Unit =
    if (!isBlockChainSynced) syncTracker.getPeersForConnection.lastOption match {
      case Some((peer, _)) =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
      case None => logger.info(s"BlockChain is not synced. There is no nodes, which we can connect with.")
    }
    else syncTracker.getPeersForConnection match {
      case coll: Vector[_] if coll.nonEmpty =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        coll.foreach { case (peer, _) =>
          logger.debug(s"Sent download request to the ${peer.socketAddress}.")
          requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
        }
      case _ => logger.info(s"BlockChain is synced. There is no nodes, which we can connect with.")
    }

  /**
    *
    * @param mTid
    * @param mId
    * @param peer
    * @param isBlockChainSynced
    */
  def receive(mTid: ModifierTypeId,
              mId: ModifierId,
              peer: ConnectedPeer,
              isBlockChainSynced: Boolean): Unit =
    if (isExpecting(mId, peer)) {
      logger.debug(s"Got new modifier with type $mTid from: ${peer.socketAddress}.")
      syncTracker.incrementReceive(peer)
      val peerExpectedModifiers: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)
      peerExpectedModifiers.get(toKey(mId)).foreach(_._1.cancel())
      val peerReceivedModifiers: Set[ModifierIdAsKey] = receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
      receivedModifiers = receivedModifiers.updated(peer.socketAddress.getAddress, peerReceivedModifiers + toKey(mId))
      expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerExpectedModifiers - toKey(mId))
    } else deliveredSpam = deliveredSpam - toKey(mId) + (toKey(mId) -> peer)

  /**
    *
    * @param id
    * @return
    */
  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)
}

object DeliveryManager {
  case object FullBlockChainSynced
  case object GetStatusTrackerPeer
}