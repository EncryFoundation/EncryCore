package encry.network

import java.net.InetAddress

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.{Fork, HistoryComparisonResult, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManager._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
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
import encry.modifiers.history.Header

class DeliveryManager(influxRef: Option[ActorRef],
                      nodeViewHolderRef: ActorRef,
                      networkControllerRef: ActorRef,
                      settings: EncryAppSettings) extends Actor with StrictLogging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  /**
    * This collection used to keep peer's who sent us headers ids.
    */
  var headersForPriorityRequest: Map[ModifierIdAsKey, Seq[InetAddress]] = Map.empty
  /**
    * This collection used to keep spam modifiers ids.
    */
  var receivedSpamModifiers: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
  /**
    * This collection used to keep received modifiers ids.
    */
  //var receivedModifiers: Map[InetAddress, Set[ModifierIdAsKey]] = Map.empty
  /**
    * This collection used to keep expecting modifiers ids.
    */
  var expectedModifiers: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
  val syncTracker: SyncTracker = SyncTracker(self, context, settings.network)

  context.system.scheduler.schedule(1.seconds, 10.seconds)(
    println(s"headersForPriorityRequest -> ${headersForPriorityRequest.size} " +
      s"receivedSpamModifiers -> ${receivedSpamModifiers.size} " +
      s"expectedModifiers -> ${expectedModifiers.map(x => x._1 -> x._2.size)}")
  )

  var tmpVar: Long = System.currentTimeMillis()

  override def preStart(): Unit =
    networkControllerRef ! RegisterMessagesHandler(Seq(ModifiersNetworkMessage.NetworkMessageTypeID), self)

  override def receive: Receive = {
    case UpdatedHistory(historyReader) =>
      logger.info(s"Got message with history. Starting normal actor's work.")
      //      context.system.scheduler.schedule(5.second, settings.network.modifierDeliverTimeCheck){
      //        println(s"Trigger CheckModifiersToDownload from context.system.scheduler.schedule")
      //        self ! CheckModifiersToDownload
      //      }
      context.system.scheduler.schedule(5.second, settings.network.updatePriorityTime.seconds)(syncTracker.updatePeersPriorityStatus())
      syncTracker.scheduleSendSyncInfo()
      context.become(basicMessageHandler(historyReader, isBlockChainSynced = false, isMining = settings.node.mining))
    case message =>
      logger.info(s"Got new message $message while awaiting history.")
  }

  def basicMessageHandler(history: EncryHistory, isBlockChainSynced: Boolean, isMining: Boolean): Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      syncTracker.updateStatus(remote, status)
      status match {
        case Unknown => logger.info("Peer status is still unknown.")
        case Younger if isBlockChainSynced => sendInvData(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => syncTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => syncTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      val expectedModifiersByPeer: Map[ModifierIdAsKey, (Cancellable, Int)] =
        expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
      if (modifierTypeId == Transaction.ModifierTypeId)
        expectedModifiers = clearExpectedModifiersCollection(expectedModifiersByPeer, toKey(modifierId), peer.socketAddress.getAddress)
      else expectedModifiersByPeer.find { case (id, (_, _)) => id == toKey(modifierId) } match {
        case Some((_, (_, attempts))) if attempts <= settings.network.maxDeliveryChecks =>
          logger.info(s"Modifier ${Algos.encode(modifierId)} needed to be requested!")
          reRequestModifier(peer, modifierTypeId, modifierId, expectedModifiersByPeer)
        case Some((modId, (_, _))) =>
          logger.info(s"Maximum number of attempts has expired. Remove modifier ${Algos.encode(modifierId)}.")
          expectedModifiers = clearExpectedModifiersCollection(expectedModifiersByPeer, modId, peer.socketAddress.getAddress)
        case _ => logger.info(s"This modifiers ${Algos.encode(modifierId)} has been already delivered. We don't need reRequest it.")
      }
    //      val modIdsFromPeer: Set[ModifierIdAsKey] = receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
    //      modIdsFromPeer.find(id => id == toKey(modifierId)) match {
    //        case Some(_) if modIdsFromPeer.size <= 1 => receivedModifiers = receivedModifiers - peer.socketAddress.getAddress
    //        case Some(_) => receivedModifiers =
    //          receivedModifiers.updated(peer.socketAddress.getAddress, modIdsFromPeer - toKey(modifierId))
    //        case None => reRequestModifier(peer, modifierTypeId, modifierId)
    //      }
    case CheckModifiersToDownload =>
    //      val requestedMods: HashSet[ModifierId] = expectedModifiers.flatMap { case (_, modIds) =>
    //        modIds.keys.map(modId => ModifierId @@ modId.toArray)
    //      }.to[HashSet]
    //nodeViewHolderRef ! CheckModifiersWithQueueSize(requestedMods.size)
    case ModifiersFromNVH(mods) if System.currentTimeMillis() - tmpVar > 10000 =>
      val requestedMods: HashSet[ModifierIdAsKey] = expectedModifiers.flatMap { case (_, modIds) => modIds.keys }.to[HashSet]
      println(s"\nModifiersFromNVH received mods: ${mods.size}\n")
      val a = mods
        .filterNot(mod => requestedMods.contains(toKey(mod._2)))
      println(s"\nModifiersFromNVH filtered ${a.size}\n")
      a.groupBy(_._1).foreach { case (modId, ids) =>
        requestDownload(modId, ids.map(_._2), history, isBlockChainSynced, isMining)
      }
      tmpVar = System.currentTimeMillis()
    case ModifiersFromNVH(mods) =>
      println(s"Got ModifiersFromNVH with ${mods.size} during if System.currentTimeMillis() - tmpVar > 10000 ")
    //      history.modifiersToDownload(settings.network.networkChunkSize - requestedMods.size, requestedMods)
    //        .groupBy(_._1)
    //        .foreach { case (modId, ids) =>
    //          println(s"CheckModifiersToDownload requestDownload")
    //          requestDownload(modId, ids.map(_._2), history, isBlockChainSynced, isMining)
    //        }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
    case DataFromPeer(message, remote) => message match {
      case ModifiersNetworkMessage((typeId, modifiers)) =>
        logger.info(s"Got ${modifiers.size} modifiers on the DM from $remote")
        val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
          modifiers.partition { case (id, _) => isSpam(id) }
        influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
        for ((id, _) <- modifiers) receive(typeId, id, remote, isBlockChainSynced)
        if (spam.nonEmpty) {
          logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
            s": ${spam.keys.map(Algos.encode)}.")
          deleteSpam(spam.keys.toSeq)
        }
        val filteredModifiers: Seq[Array[Byte]] = fm.filterNot { case (modId, _) => history.contains(modId) }.values.toSeq
        if (filteredModifiers.nonEmpty) nodeViewHolderRef ! ModifiersFromRemote(typeId, filteredModifiers)
        if (!history.isHeadersChainSynced && expectedModifiers.isEmpty) sendSync(history.syncInfo, isBlockChainSynced)
        else if (history.isHeadersChainSynced && !history.isFullChainSynced && expectedModifiers.isEmpty) {
          println(s"Trigger CheckModifiersToDownload from DataFromPeer ${expectedModifiers.size}")
          //self ! CheckModifiersToDownload
        }
      case _ => logger.info(s"DeliveryManager got invalid type of DataFromPeer message!")
    }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: ModifierId, previousModifier: Option[ModifierId]) =>
      if (previousModifier.isDefined && isBlockChainSynced)
        priorityRequest(modifierTypeId, modifiersId, previousModifier.get, history, isBlockChainSynced, isMining)
      else {
        println(s"DownloadRequest requestDownload")
        requestDownload(modifierTypeId, Seq(modifiersId), history, isBlockChainSynced, isMining)
      }
    case SendLocalSyncInfo =>
      if (syncTracker.elapsedTimeSinceLastSync < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else sendSync(history.syncInfo, isBlockChainSynced)
    case GetSyncTrackerPeer => sender() ! syncTracker.statuses
    case FullBlockChainIsSynced =>
      logger.info("FullBlockChainIsSynced on delivery manager!")
      context.become(basicMessageHandler(history, isBlockChainSynced = true, isMining))
    case StartMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = true))
    case DisableMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = false))
    case UpdatedHistory(historyReader) => context.become(basicMessageHandler(historyReader, isBlockChainSynced, isMining))
    case message => logger.info(s"Got strange message $message on DeliveryManager.")
  }

  /**
    * If node is not synced, send sync info to random peer, otherwise to all known peers.
    *
    * @param syncInfo           - sync info
    * @param isBlockChainSynced - current block chain status
    */
  def sendSync(syncInfo: EncrySyncInfo, isBlockChainSynced: Boolean): Unit =
    if (isBlockChainSynced) syncTracker.peersToSyncWith.foreach(peer => peer.handlerRef ! SyncInfoNetworkMessage(syncInfo))
    else Random.shuffle(syncTracker.peersToSyncWith).headOption.foreach(peer => peer.handlerRef ! SyncInfoNetworkMessage(syncInfo))

  /**
    * Send request to 'peer' with modifiers ids of type 'modifierTypeId'.
    * We can do this activity only if 'peer' status != Younger.
    * If current chain isn't synced and mining is off, we can't request transactions, otherwise can do.
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
    val firstCondition: Boolean = mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining
    val secondCondition: Boolean = mTypeId != Transaction.ModifierTypeId
    val thirdCondition: Boolean = syncTracker.statuses.get(peer.socketAddress.getAddress)
      .exists { case (comrResult, _, _) => comrResult != Younger && comrResult != Fork }

    if ((firstCondition || secondCondition) && thirdCondition) {
      val requestedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)

      //val receivedModifiersByPeer: Set[ModifierIdAsKey] = receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)

      val notYetRequested: Seq[ModifierId] = modifierIds.filter(id => !history.contains(id)
        && /*!receivedModifiersByPeer.contains(toKey(id))*/ !requestedModifiersFromPeer.contains(toKey(id)))

      //      val notYetRequested: Seq[ModifierId] = modifierIds.filterNot(id => (history.contains(id)
      //        || /*!receivedModifiersByPeer.contains(toKey(id))*/ requestedModifiersFromPeer.contains(toKey(id))))

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

  /**
    * Re-ask 'modifierId' from 'peer' if needed. We will do this only if awaiting this modifier from 'peer'
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
      case Some((timer, attempts)) =>
        syncTracker.statuses.find { case (innerPeerAddr, (cResult, _, _)) =>
          innerPeerAddr == peer.socketAddress.getAddress && cResult != Younger && cResult != Fork
        }.foreach {
          case (_, (_, _, cP)) =>
            cP.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> Seq(modId))
            logger.debug(s"Re-asked ${peer.socketAddress} and handler: ${peer.handlerRef} for modifier of type: " +
              s"$mTypeId with id: ${Algos.encode(modId)}")
            syncTracker.incrementRequest(peer)
            timer.cancel()
            expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerRequests.updated(
              toKey(modId),
              context.system.scheduler
                .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, modId)) -> (attempts + 1)
            ))
          case _ => logger.info(s"Tried to re-ask modifier ${Algos.encode(modId)}, but this id not needed from this peer")
            //expectedModifiers = clearExpectedModifiersCollection(peerRequests, toKey(modId), peer.socketAddress.getAddress)
          //expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerRequests - toKey(modId))
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
          networkControllerRef ! SendToNetwork(InvNetworkMessage(mTid -> mods), SendToPeer(peer))
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
    * 2) Choose peer with highest priority.
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
    if (!isBlockChainSynced) syncTracker.getPeersForConnection.lastOption match {
      case Some((_, (_, _, cP))) =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        requestModifies(history, cP, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
      case None => logger.info(s"BlockChain is not synced. There is no nodes, which we can connect with.")
    }
    else syncTracker.getPeersForConnection match {
      case coll: Vector[_] if coll.nonEmpty =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        coll.foreach { case (_, (_, _, cP)) =>
          logger.debug(s"Sent download request to the ${cP.socketAddress}.")
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
      logger.debug(s"Got new modifier with type $mTid from: ${peer.socketAddress}.")
      syncTracker.incrementReceive(peer)
      val peerExpectedModifiers: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)
      peerExpectedModifiers.get(toKey(mId)).foreach(_._1.cancel())
      //val peerReceivedModifiers: Set[ModifierIdAsKey] = receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
      //receivedModifiers = receivedModifiers.updated(peer.socketAddress.getAddress, peerReceivedModifiers + toKey(mId))
      logger.info(s"expectedModifiers -> ${expectedModifiers.map(x => x._1 -> x._2.size)} BEFORE")
      expectedModifiers = clearExpectedModifiersCollection(peerExpectedModifiers, toKey(mId), peer.socketAddress.getAddress)
      //expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerExpectedModifiers - toKey(mId))
      logger.info(s"expectedModifiers -> ${expectedModifiers.map(x => x._1 -> x._2.size)} AFTRE")
      if (isBlockChainSynced && mTid == Header.modifierTypeId) {
        logger.info(s"Received header with id: ${Algos.encode(mId)} from peer: ${peer.socketAddress.getAddress}")
        headersForPriorityRequest = headersForPriorityRequest
          .updated(toKey(mId), headersForPriorityRequest.getOrElse(toKey(mId), Seq.empty) :+ peer.socketAddress.getAddress)
        logger.info(s"After updating headersForPriorityRequest contains ${headersForPriorityRequest.get(toKey(mId))}")
      }
    } else receivedSpamModifiers = receivedSpamModifiers - toKey(mId) + (toKey(mId) -> peer)

  /**
    * Transform modifier id to WrappedArray of bytes
    *
    * @param id - modifier id which will be transform to WrappedArray of bytes
    * @return transformed modifier id
    */
  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)

  /**
    *
    * @param peerCollection
    * @param modifierId
    * @param peer
    * @return
    */
  def clearExpectedModifiersCollection(peerCollection: Map[ModifierIdAsKey, (Cancellable, Int)],
                                       modifierId: ModifierIdAsKey,
                                       peer: InetAddress): Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = {
    val collectionWithoutModId: Map[ModifierIdAsKey, (Cancellable, Int)] = peerCollection - modifierId
    collectionWithoutModId match {
      case coll: Map[_, _] if coll.nonEmpty => expectedModifiers.updated(peer, coll)
      case _ => expectedModifiers - peer
    }
  }
}

object DeliveryManager {

  case object FullBlockChainIsSynced

  case object GetSyncTrackerPeer

  case class CheckModifiersWithQueueSize(size: Int)

  case class ModifiersFromNVH(mods: Seq[(ModifierTypeId, ModifierId)])

  def props(influxRef: Option[ActorRef],
            nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: EncryAppSettings): Props =
    Props(new DeliveryManager(influxRef, nodeViewHolderRef, networkControllerRef, settings))
}