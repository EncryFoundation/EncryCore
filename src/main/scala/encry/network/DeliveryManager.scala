package encry.network

import java.net.InetAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, PoisonPill, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.DeliveryManager._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.EncryHistory
import encry.settings.EncryAppSettings
import scala.concurrent.duration._
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Random
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.view.mempool.Mempool.RequestForTransactions
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{InvNetworkMessage, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scala.concurrent.ExecutionContextExecutor

class DeliveryManager(influxRef: Option[ActorRef],
                      nodeViewHolderRef: ActorRef,
                      networkControllerRef: ActorRef,
                      settings: EncryAppSettings,
                      memoryPoolRef: ActorRef) extends Actor with StrictLogging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  implicit val exCon: ExecutionContextExecutor = context.dispatcher
  /**
    * If block chain is synced, we will put all headers ids and peers who sent us this headers to this collection
    * in order to ask payload directly from peers who sent us appropriate header.
    */
  var headersForPriorityRequest: Map[ModifierIdAsKey, Seq[InetAddress]] = Map.empty
  /**
    * Collection with spam modifiers.
    * Modifier considered spam if we receive it but it doesn't contain in expected modifiers collection.
    */
  var receivedSpamModifiers: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  /**
    * Collection of received modifiers ids.
    * Modifier considered received if we sent request for it and received it in special period.
    */
  var receivedModifiers: HashSet[ModifierIdAsKey] = HashSet.empty[ModifierIdAsKey]
  /**
    * Collection of expected modifiers ids.
    * Modifier considered expected if we sent request for it.
    */
  var expectedModifiers: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
  val syncTracker: SyncTracker = SyncTracker(self, context, settings.network)

  override def preStart(): Unit = {
    networkControllerRef ! RegisterMessagesHandler(
      Seq(ModifiersNetworkMessage.NetworkMessageTypeID -> "ModifiersNetworkMessage"), self)
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
  }

  override def receive: Receive = {
    case UpdatedHistory(historyReader) =>
      logger.info(s"Got message with history. Starting normal actor's work.")
      context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! CheckModifiersToDownload)
      context.system.scheduler.schedule(
        0.second,
        (settings.network.deliveryTimeout._1 * settings.network.maxDeliveryChecks).seconds)(syncTracker.updatePeersPriorityStatus())
      syncTracker.scheduleSendSyncInfo()
      context.become(basicMessageHandler(historyReader, isBlockChainSynced = false, isMining = settings.node.mining))
    case HandshakedPeer(remote) => syncTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => syncTracker.clearStatus(remote)
    case message => logger.info(s"Got new message $message while awaiting history.")
  }

  def basicMessageHandler(history: EncryHistory, isBlockChainSynced: Boolean, isMining: Boolean): Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      syncTracker.updateStatus(remote, status)
      status match {
        case Unknown => logger.info("Peer status is still unknown.")
        case Younger | Fork if isBlockChainSynced => sendInvData(remote, status, extOpt)
        case _ =>
      }
    case CheckDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      checkDelivery(peer, modifierTypeId, modifierId)
    case HandshakedPeer(remote) => syncTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => syncTracker.clearStatus(remote)
    case CheckModifiersToDownload =>
      val currentQueue: HashSet[ModifierIdAsKey] =
        expectedModifiers.flatMap { case (_, modIds) => modIds.keys }.to[HashSet]
      logger.info(s"Current queue: ${currentQueue.map(elem => Algos.encode(elem.toArray)).mkString(",")}")
      val newIds: Seq[(ModifierTypeId, ModifierId)] =
        history.modifiersToDownload(
          settings.network.networkChunkSize - currentQueue.size,
          currentQueue.map(elem => ModifierId @@ elem.toArray)
        ).filterNot(modId => currentQueue.contains(toKey(modId._2)))
      logger.info(s"newIds: ${newIds.map(elem => Algos.encode(elem._2)).mkString(",")}")
      if (newIds.nonEmpty) newIds.groupBy(_._1).foreach {
        case (modId: ModifierTypeId, ids: Seq[(ModifierTypeId, ModifierId)]) =>
          requestDownload(modId, ids.map(_._2), history, isBlockChainSynced, isMining)
      }
      context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! CheckModifiersToDownload)
    case SemanticallySuccessfulModifier(mod) => receivedModifiers -= toKey(mod.id)
    case SemanticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
    case SyntacticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
    case SuccessfulTransaction(_) => //do nothing
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierTypeId != Transaction.modifierTypeId) logger.info(s"Got RequestFromLocal on NVSH from $sender with " +
        s"ids of type: $modifierTypeId. Number of ids is: ${modifierIds.size}. Sending request from local to DeliveryManager.")
      if (modifierIds.nonEmpty) requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
    case RequestForTransactions(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
    case DataFromPeer(message, remote) => message match {
      case ModifiersNetworkMessage((typeId, modifiers)) =>
        if (typeId != Transaction.modifierTypeId)
          logger.info(s"Got ${modifiers.size} modifiers on the DM of type: $typeId from $remote with id:" +
            s"${modifiers.keys.map(Algos.encode).mkString(",")}")
        influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
        for ((id, _) <- modifiers) receive(typeId, id, remote, isBlockChainSynced)
        val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
          modifiers.partition { case (id, _) => isSpam(id) }
        if (spam.nonEmpty) {
          if (typeId != Transaction.modifierTypeId)
            logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
            s": ${spam.keys.map(Algos.encode)}.")
          receivedSpamModifiers = Map.empty
        }
        val filteredModifiers: Seq[Array[Byte]] = fm.filterNot { case (modId, _) => {
          logger.info(s"History contains ${Algos.encode(modId)}: ${history.contains(modId)}")
          history.contains(modId)
        } }.values.toSeq
        if (filteredModifiers.nonEmpty && typeId == Transaction.modifierTypeId)
          memoryPoolRef ! ModifiersFromRemote(typeId, filteredModifiers)
        else if (filteredModifiers.nonEmpty) {
          logger.info(s"Sending modifiers form network of type:$typeId from $remote to NVH. ")
          nodeViewHolderRef ! ModifiersFromRemote(typeId, filteredModifiers)
        }
        if (!history.isHeadersChainSynced && expectedModifiers.isEmpty) sendSync(history.syncInfo, isBlockChainSynced)
      case _ => logger.info(s"DeliveryManager got invalid type of DataFromPeer message!")
    }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: ModifierId, previousModifier: Option[ModifierId]) =>
      if (modifierTypeId != Transaction.modifierTypeId)
        logger.info(s"DownloadRequest for mod ${Algos.encode(modifiersId)} of type: $modifierTypeId prev mod: " +
        s"${previousModifier.map(Algos.encode)}")
      if (previousModifier.isDefined && isBlockChainSynced) {
        logger.info(s"Sending this donwload request! for modfiier: ${Algos.encode(modifiersId)}")
        priorityRequest(modifierTypeId, modifiersId, previousModifier.get, history, isBlockChainSynced, isMining)
      }
      else requestDownload(modifierTypeId, Seq(modifiersId), history, isBlockChainSynced, isMining)
    case SendLocalSyncInfo =>
      if (syncTracker.elapsedTimeSinceLastSync < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else sendSync(history.syncInfo, isBlockChainSynced)
    case FullBlockChainIsSynced => context.become(basicMessageHandler(history, isBlockChainSynced = true, isMining))
    case StartMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = true))
    case DisableMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = false))
    case UpdatedHistory(historyReader) => context.become(basicMessageHandler(historyReader, isBlockChainSynced, isMining))
    case message => logger.info(s"Got strange message $message(${message.getClass}) on DeliveryManager from $sender")
  }

  /**
    * This function check if modifier has received or not.
    * If modifier has transaction type id, it won't be re-asked.
    * If we still have no this modifier and number of attempts have no expired, we will re-ask it.
    * If we still have no this modifier and number of attempts have expired, we will remove it from expected modifiers collection.
    * Otherwise - do nothing.
    *
    * @param peer           - peer, from whom we are expecting modifier
    * @param modifierTypeId - type of expected modifier
    * @param modifierId     - expected modifier id
    */
  def checkDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val expectedModifiersByPeer: Map[ModifierIdAsKey, (Cancellable, Int)] =
      expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
    if (modifierTypeId == Transaction.modifierTypeId)
      expectedModifiers = clearExpectedModifiersCollection(expectedModifiersByPeer, toKey(modifierId), peer.socketAddress.getAddress)
    else expectedModifiersByPeer.find { case (id, (_, _)) => id == toKey(modifierId) } match {
      case Some((_, (_, attempts))) if attempts <= settings.network.maxDeliveryChecks =>
        logger.info(s"Modifier ${Algos.encode(modifierId)} needed to be requested from $peer!")
        reRequestModifier(peer, modifierTypeId, modifierId, expectedModifiersByPeer)
      case Some((modId, (_, _))) =>
        logger.info(s"Maximum number of attempts has expired. Remove modifier ${Algos.encode(modifierId)} from $peer.")
        expectedModifiers = clearExpectedModifiersCollection(expectedModifiersByPeer, modId, peer.socketAddress.getAddress)
      case _ =>
        logger.info(s"This modifiers ${Algos.encode(modifierId)} is not contained in expectedModifiers collection from $peer.")
    }
  }
  /**
    * If node is not synced, send sync info to random peer, otherwise to all known peers.
    *
    * @param syncInfo           - sync info
    * @param isBlockChainSynced - current block chain status
    */
  def sendSync(syncInfo: SyncInfo, isBlockChainSynced: Boolean): Unit =
    if (isBlockChainSynced) syncTracker.peersToSyncWith.foreach(peer => peer.handlerRef ! SyncInfoNetworkMessage(syncInfo))
    else Random.shuffle(syncTracker.peersToSyncWith).headOption.foreach(peer => peer.handlerRef ! SyncInfoNetworkMessage(syncInfo))

  /**
    * Send request to 'peer' with modifiers ids of type 'modifierTypeId'.
    * We can do this activity only if 'peer' status != Younger.
    * If current chain isn't synced and mining is off, we can't request transactions, otherwise can.
    *
    * We should filter our requesting modifiers to avoid request repeated modifiers.
    *
    * @param history            - current history reader
    * @param peer               - peer, whom message will be send
    * @param mTypeId            - modifier type id
    * @param modifierIds        - modifiers ids
    * @param isBlockChainSynced - current block chain status
    * @param isMining           - current mining status
    */
  def requestModifies(history: EncryHistory,
                      peer: ConnectedPeer,
                      mTypeId: ModifierTypeId,
                      modifierIds: Seq[ModifierId],
                      isBlockChainSynced: Boolean,
                      isMining: Boolean): Unit = {
    val firstCondition: Boolean = mTypeId == Transaction.modifierTypeId && isBlockChainSynced && isMining
    val secondCondition: Boolean = mTypeId != Transaction.modifierTypeId
    val thirdCondition: Boolean =
      if (!isBlockChainSynced) syncTracker.statuses.get(peer.socketAddress.getAddress)
        .exists { case (comrResult, _, _) => comrResult != Younger }
      else syncTracker.statuses.contains(peer.socketAddress.getAddress)
    if (mTypeId != Transaction.modifierTypeId)
      logger.info(s"Got requestModifier for modifiers of type: $mTypeId to $peer with modifiers ${modifierIds.size}." +
        s" Try to check conditions: $firstCondition -> $secondCondition -> $thirdCondition.")
    if ((firstCondition || secondCondition) && thirdCondition) {
      val requestedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)

      val notYetRequested: Seq[ModifierId] = modifierIds.filter(id => !history.contains(id)
        && !requestedModifiersFromPeer.contains(toKey(id)) && !receivedModifiers.contains(toKey(id)))

      if (notYetRequested.nonEmpty) {
        if (mTypeId != Transaction.modifierTypeId)
          logger.info(s"Send request to ${peer.socketAddress.getAddress} for ${notYetRequested.size} modifiers of type $mTypeId ")
        peer.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> notYetRequested)
        syncTracker.incrementRequestForNModifiers(peer, notYetRequested.size)
        val requestedModIds: Map[ModifierIdAsKey, (Cancellable, PeerPriorityStatus)] =
          notYetRequested.foldLeft(requestedModifiersFromPeer) { case (rYet, id) =>
            rYet.updated(toKey(id),
              context.system
                .scheduler.scheduleOnce(settings.network.deliveryTimeout)(self ! CheckDelivery(peer, mTypeId, id)) -> 1)
          }
        expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, requestedModIds)
      }
    }
  }
  /**
    * Scheduler for executing checkDelivery function.
    *
    * @param peer           - peer, from whom we are expecting modifier
    * @param modifierTypeId - type of expected modifier
    * @param modifierId     - expected modifier id
    */
  def schedulerChecker(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit =
    checkDelivery(peer, modifierTypeId, modifierId)
  /**
    * Re-ask 'modifierId' from 'peer' if needed. We will do this only if we are expecting these modifier from 'peer'
    * and if number of attempts doesn't expired yet.
    * This activity will update timer on re-asked modifier.
    *
    * @param peer    - peer, whom message will be send
    * @param mTypeId - modifier type id
    * @param modId   - re-asked modifier id
    */
  def reRequestModifier(peer: ConnectedPeer,
                        mTypeId: ModifierTypeId,
                        modId: ModifierId,
                        peerRequests: Map[ModifierIdAsKey, (Cancellable, Int)]): Unit =
    peerRequests.get(toKey(modId)) match {
      case Some((_, attempts)) => syncTracker.statuses.find { case (innerPeerAddr, (cResult, _, _)) =>
        innerPeerAddr == peer.socketAddress.getAddress && cResult != Younger
      } match {
        case Some((_, (_, _, cP))) =>
          cP.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> Seq(modId))
          logger.info(s"Re-asked ${peer.socketAddress} and handler: ${peer.handlerRef} for modifier of type: " +
            s"$mTypeId with id: ${Algos.encode(modId)}. Attempts: $attempts")
          syncTracker.incrementRequest(peer)
          expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerRequests.updated(
            toKey(modId),
            context.system.scheduler
              .scheduleOnce(settings.network.deliveryTimeout)(self ! CheckDelivery(peer, mTypeId, modId)) -> (attempts + 1)
          ))
        case None =>
          expectedModifiers = clearExpectedModifiersCollection(peerRequests, toKey(modId), peer.socketAddress.getAddress)
          logger.info(s"Tried to re-ask modifier ${Algos.encode(modId)}, but this id not needed from this peer")
      }
      case _ => logger.info(s"There is no such modifier ${Algos.encode(modId)} in expected collection.")
    }
  /**
    * Check 'expectedModifiers' for awaiting modifier with id 'mId' from 'peer'
    *
    * @param mId  - id of checkable modifier
    * @param peer - peer from which we possibly expecting modifier
    * @return 'true' if we are expecting this modifier from this peer otherwise 'false'
    */
  def isExpecting(mId: ModifierId, peer: ConnectedPeer): Boolean =
    expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty).contains(toKey(mId))
  /**
    * Clear the 'receivedSpamModifiers' collection
    *
    * @param mIds - sequence of modifiers ids which will be deleted from spam collection
    */
  def deleteSpam(mIds: Seq[ModifierId]): Unit = for (id <- mIds) receivedSpamModifiers -= toKey(id)
  /**
    * Check receivedSpamModifiers for contains received modifier
    *
    * @param mId - checkable modifier
    * @return 'true' if received modifier is in spam collection otherwise 'false'
    */
  def isSpam(mId: ModifierId): Boolean = receivedSpamModifiers.contains(toKey(mId))
  /**
    * Send inv data to the 'peer'.
    *
    * @param peer              - peer whom will send a message
    * @param status            - current peer's status
    * @param dataForInvMessage - data for inv message
    */
  def sendInvData(peer: ConnectedPeer,
                  status: HistoryComparisonResult,
                  dataForInvMessage: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = dataForInvMessage match {
    case Some(data) =>
      data.groupBy(_._1).mapValues(_.map(_._2)).foreach {
        case (mTid, mods) if mods.size <= settings.network.maxInvObjects =>
          logger.info(s"Send to peer ${peer} inv msg with mods: ${mods.map(Algos.encode).mkString(",")}")
          peer.handlerRef ! InvNetworkMessage(mTid -> mods)
        case (_, mods) => logger.info(s"Tried to send inv message with size ${mods.size}. Current size is redundant.")
      }
    case None => logger.info(s"dataForInvMessage is empty for: $peer. Peer's status is: $status.")
  }
  /**
    * This function provides request with priority status. This means, that in priority we will ask peer who sent us
    * a header to send us payload. If we can't connect to this peer we will call 'requestDownload' function
    *
    * @param modifierTypeId     - modifier type id
    * @param modifierIds        - requesting payload id
    * @param headerId           - payload's header's id
    * @param history            - current history state
    * @param isBlockChainSynced - current block chain status
    * @param isMining           - current mining status
    */
  def priorityRequest(modifierTypeId: ModifierTypeId,
                      modifierIds: ModifierId,
                      headerId: ModifierId,
                      history: EncryHistory,
                      isBlockChainSynced: Boolean,
                      isMining: Boolean): Unit = headersForPriorityRequest.get(toKey(headerId)) match {
    case Some(addresses) if addresses.nonEmpty =>
      logger.info(s"Trying to make priority request to payload for header(${Algos.encode(headerId)}). " +
        s"Addresses: $addresses")
      syncTracker.statuses.find(_._1 == addresses.head) match {
        case Some((_, (_, _, cP))) =>
          logger.info(s"Find handler for address: ${addresses.head}")
          headersForPriorityRequest = headersForPriorityRequest - toKey(headerId)
          requestModifies(history, cP, modifierTypeId, Seq(modifierIds), isBlockChainSynced, isMining)
        case None => requestDownload(modifierTypeId, Seq(modifierIds), history, isBlockChainSynced, isMining)
      }
    case _ => requestDownload(modifierTypeId, Seq(modifierIds), history, isBlockChainSynced, isMining)
  }
  /**
    * If node is not synced, `requestDownload` sends request for the one peer which will be find by 2 criteria:
    * 1) HistoryComparisonResult != Younger.
    * 2) Choose random peer with non bad priority.
    * Otherwise this function sends requests for all known peers selected by 1-st criterion as above.
    *
    * If there are no any peers, request won't be sent.
    *
    * @param modifierTypeId     - modifier type id
    * @param modifierIds        - modifier id
    * @param history            - current history state
    * @param isBlockChainSynced - current block chain status
    * @param isMining           - current mining status
    */
  def requestDownload(modifierTypeId: ModifierTypeId,
                      modifierIds: Seq[ModifierId],
                      history: EncryHistory,
                      isBlockChainSynced: Boolean,
                      isMining: Boolean): Unit =
    if (!isBlockChainSynced) {
      logger.info(s"Function - request modifiers. syncTracker's peerCollection is: ${syncTracker.statuses}")
      logger.info(s"Function - request modifiers. syncTracker's getPeersForConnection is: ${syncTracker.peersForDownloadRequest}")
      val (withBadNodes, withoutBadNodes) = syncTracker.peersForDownloadRequest.partition {
        case (_, (_, priority, _)) => priority == SyncTracker.PeerPriorityStatus.BadNode
      }
      val resultedPeerCollection: Vector[(InetAddress, (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer))] =
        if (withBadNodes.nonEmpty) withoutBadNodes :+ Random.shuffle(withBadNodes).head
        else withoutBadNodes
      logger.info(s"Blockchain is not synced. acceptedPeers: $resultedPeerCollection")
      if (resultedPeerCollection.nonEmpty) {
        val shuffle = Random.shuffle(resultedPeerCollection)
        val cP = shuffle.last._2._3
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        if (modifierTypeId != Transaction.modifierTypeId)
          logger.info(s"requestModifies for peer ${cP.socketAddress.getAddress} for mods: ${modifierIds.map(Algos.encode).mkString(",")}")
        requestModifies(history, cP, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
      } else logger.info(s"BlockChain is not synced. There is no nodes, which we can connect with.")
    }
    else syncTracker.peersForDownloadRequest match {
      case coll: Vector[_] if coll.nonEmpty =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        coll.foreach { case (_, (_, _, cP)) =>
          if (modifierTypeId != Transaction.modifierTypeId)
            logger.info(s"Sent download request to the ${cP.socketAddress} to modifiers of type: $modifierTypeId." +
              s"\n Modifiers are: ${modifierIds.map(Algos.encode).mkString(",")}.")
          requestModifies(history, cP, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
        }
      case _ => logger.info(s"BlockChain is synced. There is no nodes, which we can connect with.")
    }
  /**
    * Handle received modifier. We will process received modifier only if we are expecting this on.
    *
    * @param mTid               - modifier type id
    * @param mId                - modifier id
    * @param peer               - peer who sent modifier
    * @param isBlockChainSynced - current chain status
    */
  def receive(mTid: ModifierTypeId,
              mId: ModifierId,
              peer: ConnectedPeer,
              isBlockChainSynced: Boolean): Unit =
    if (isExpecting(mId, peer)) {
      if (mTid != Transaction.modifierTypeId)
        logger.info(s"Got new modifier with type $mTid from: ${peer.socketAddress}. with id ${Algos.encode(mId)}")
      syncTracker.incrementReceive(peer)
      val peerExpectedModifiers: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)
      peerExpectedModifiers.get(toKey(mId)).foreach(_._1.cancel())
      if (mTid != Transaction.modifierTypeId) receivedModifiers += toKey(mId)
      expectedModifiers = clearExpectedModifiersCollection(peerExpectedModifiers, toKey(mId), peer.socketAddress.getAddress)
      if (isBlockChainSynced && mTid == Header.modifierTypeId) {
        logger.info(s"Received header with id: ${Algos.encode(mId)} from peer: ${peer.socketAddress.getAddress}")
        headersForPriorityRequest = headersForPriorityRequest
          .updated(toKey(mId), headersForPriorityRequest.getOrElse(toKey(mId), Seq.empty) :+ peer.socketAddress.getAddress)
      }
      if (expectedModifiers.isEmpty) self ! SendLocalSyncInfo
    } else {
      receivedSpamModifiers = receivedSpamModifiers - toKey(mId) + (toKey(mId) -> peer)
      syncTracker.decrementRequest(peer)
    }
  /**
    * Transform modifier id to WrappedArray.ofBytes
    *
    * @param id - modifier id which will be transform to WrappedArray of bytes.
    * @return transformed modifier id
    */
  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)
  /**
    * This function gets collection of current expected modifiers from 'peer' and modifier, which
    * will be removed from received collection as a parameters.
    * If expected modifiers collection will contain other modifiers even after removing,
    * this function will return collection of expectedModifiers with updated 'peer' expected collection
    * otherwise it will return expectedModifiers collection without 'peer'.
    *
    * @param expectedModifiersFromPeer - collection of expected modifiers from 'peer'
    * @param modifierId                - modifier id, which will be removed from 'expectedModifiersFromPeer'
    * @param peer                      - 'peer' from which expected modifiers collection we remove received modifier
    * @return - expectedModifiers collection without 'peer' or expectedModifiers with updated 'peer' expected collection
    */
  def clearExpectedModifiersCollection(expectedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)],
                                       modifierId: ModifierIdAsKey,
                                       peer: InetAddress): Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = {
    val collectionWithoutModId: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiersFromPeer - modifierId
    collectionWithoutModId match {
      case coll: Map[_, _] if coll.nonEmpty => expectedModifiers.updated(peer, coll)
      case _ => expectedModifiers - peer
    }
  }
}

object DeliveryManager {

  case object FullBlockChainIsSynced
  case class  CheckModifiersWithQueueSize(size: Int)
  final case class CheckDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId)

  def props(influxRef: Option[ActorRef],
            nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: EncryAppSettings,
            memoryPoolRef: ActorRef): Props =
    Props(new DeliveryManager(influxRef, nodeViewHolderRef, networkControllerRef, settings, memoryPoolRef))

  class DeliveryManagerPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case RequestFromLocal(_, _, _) => 0

        case OtherNodeSyncingStatus(_, _, _) => 1

        case DataFromPeer(msg: ModifiersNetworkMessage, _) =>
          msg match {
            case ModifiersNetworkMessage((typeId, _)) if typeId != Transaction.modifierTypeId => 1
            case _ => 3
          }
        case RequestForTransactions(_, _, _) => 3
        case PoisonPill => 4
        case _ => 2
      })
}