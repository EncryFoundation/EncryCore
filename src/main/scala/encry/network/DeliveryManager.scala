package encry.network

import java.net.InetAddress

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
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

import scala.concurrent.duration._
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Random
import BasicMessagesRepo._
import encry.modifiers.history.{Header, Payload}
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus

import scala.concurrent.{ExecutionContextExecutor, Future}

class DeliveryManager(influxRef: Option[ActorRef],
                      nodeViewHolderRef: ActorRef,
                      networkControllerRef: ActorRef,
                      settings: EncryAppSettings) extends Actor with StrictLogging {

  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

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
  var receivedModifiers: HashSet[ModifierIdAsKey] = HashSet.empty[ModifierIdAsKey]
  /**
    * This collection used to keep expecting modifiers ids.
    */
  var expectedModifiers: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
  val syncTracker: SyncTracker = SyncTracker(self, context, settings.network)

  context.system.scheduler.schedule(1.seconds, 10.seconds)(
    logger.info(s"headersForPriorityRequest -> ${headersForPriorityRequest.size} " +
      s"receivedSpamModifiers -> ${receivedSpamModifiers.size} " +
      s"expectedModifiers -> ${expectedModifiers.map(x => x._1 -> x._2.size)}" +
      s" receivedModifiers -> ${receivedModifiers.size}")
  )

  var tmpVar: Long = System.currentTimeMillis()

  override def preStart(): Unit = {
    networkControllerRef ! RegisterMessagesHandler(Seq(ModifiersNetworkMessage.NetworkMessageTypeID), self)
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
  }

  override def receive: Receive = {
    case UpdatedHistory(historyReader) =>
      logger.info(s"Got message with history. Starting normal actor's work.")
      context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck) {
        logger.info(s"Trigger CheckModifiersToDownload from context.system.scheduler.schedule")
        self ! CheckModifiersToDownload
      }
      context.system.scheduler.schedule(0.second, (settings.network.deliveryTimeout._1 * settings.network.maxDeliveryChecks).seconds) {
        syncTracker.updatePeersPriorityStatus()
      }
      syncTracker.scheduleSendSyncInfo()
      context.become(basicMessageHandler(historyReader, isBlockChainSynced = false, isMining = settings.node.mining))
    case HandshakedPeer(remote) => syncTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => syncTracker.clearStatus(remote)
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
    case CheckModifiersToDownload =>
      val currentQueue: HashSet[ModifierIdAsKey] =
        expectedModifiers.flatMap { case (_, modIds) => modIds.keys }.to[HashSet]
      //logger.info(s"${expectedModifiers.map(x => x._1 -> x._2.map(k => Algos.encode(k._1.toArray))).mkString(",")}")
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
      context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck) {
        logger.info(s"Trigger CheckModifiersToDownload from context.system.scheduler.schedule")
        self ! CheckModifiersToDownload
      }
    case SemanticallySuccessfulModifier(mod) =>
      receivedModifiers -= toKey(mod.id)
      logger.info(s"receivedModifiers size: ${receivedModifiers.size}")
    case SemanticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
    case SyntacticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
    case SuccessfulTransaction(_) => //do nothing
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
    case DataFromPeer(message, remote) => message match {
      case ModifiersNetworkMessage((typeId, modifiers)) =>
        logger.info(s"Got ${modifiers.size} modifiers on the DM from $remote with id: ${modifiers.keys.map(Algos.encode).mkString(",")}")
        influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
        for ((id, _) <- modifiers) receive(typeId, id, remote, isBlockChainSynced)
        val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
          modifiers.partition { case (id, _) => isSpam(id) }
        if (spam.nonEmpty) {
          logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
            s": ${spam.keys.map(Algos.encode)}.")
          receivedSpamModifiers = Map.empty
        }
        val filteredModifiers: Seq[Array[Byte]] = fm.filterNot { case (modId, _) => history.contains(modId) }.values.toSeq
        if (filteredModifiers.nonEmpty) nodeViewHolderRef ! ModifiersFromRemote(typeId, filteredModifiers)
        if (!history.isHeadersChainSynced && expectedModifiers.isEmpty) sendSync(history.syncInfo, isBlockChainSynced)
        else if (history.isHeadersChainSynced && !history.isFullChainSynced && expectedModifiers.isEmpty) {
          logger.info(s"Trigger CheckModifiersToDownload from DataFromPeer ${expectedModifiers.size}")
          //self ! CheckModifiersToDownload
        }
      case _ => logger.info(s"DeliveryManager got invalid type of DataFromPeer message!")
    }
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: ModifierId, previousModifier: Option[ModifierId]) =>
      if (previousModifier.isDefined && isBlockChainSynced)
        priorityRequest(modifierTypeId, modifiersId, previousModifier.get, history, isBlockChainSynced, isMining)
      else {
        logger.info(s"DownloadRequest requestDownload")
        requestDownload(modifierTypeId, Seq(modifiersId), history, isBlockChainSynced, isMining)
      }
    case SendLocalSyncInfo =>
      if (syncTracker.elapsedTimeSinceLastSync < settings.network.syncInterval.toMillis / 2)
        logger.info("Trying to send sync info too often")
      else sendSync(history.syncInfo, isBlockChainSynced)
    case FullBlockChainIsSynced =>
      logger.info("FullBlockChainIsSynced on delivery manager!")
      context.become(basicMessageHandler(history, isBlockChainSynced = true, isMining))
    case StartMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = true))
    case DisableMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = false))
    case UpdatedHistory(historyReader) => context.become(basicMessageHandler(historyReader, isBlockChainSynced, isMining))
    case message => logger.info(s"Got strange message $message(${message.getClass}) on DeliveryManager. from $sender()")
  }

  def checkDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val expectedModifiersByPeer: Map[ModifierIdAsKey, (Cancellable, Int)] =
      expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
    logger.info(s"PEER -> $peer")
    //    logger.info(s"${expectedModifiersByPeer.map(x => Algos.encode(x._1.toArray)).mkString(",")} - expected mods by peer")
    //    logger.info(s"${expectedModifiers.map(x => x._1 -> x._2.map(k => Algos.encode(k._1.toArray))).mkString(",")}")
    if (modifierTypeId == Transaction.ModifierTypeId)
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
    val thirdCondition =
      if (!isBlockChainSynced) syncTracker.statuses.get(peer.socketAddress.getAddress)
        .exists { case (comrResult, _, _) => comrResult != Younger && comrResult != Fork }
      else syncTracker.statuses.contains(peer.socketAddress.getAddress)

        //.exists { case (comrResult, _, _) => comrResult != Fork }
    //    val thirdCondition: Boolean = syncTracker.statuses.get(peer.socketAddress.getAddress)
    //      .exists { case (comrResult, _, _) => comrResult != Younger && comrResult != Fork }
    logger.info("===============requestModifies============\n " +
      s"firstCondition: $firstCondition\n " +
      s"secondCondition: $secondCondition\n " +
      s"thirdCondition: $thirdCondition ")

    logger.info(s"PEEEER -> $peer")
    logger.info(s"${syncTracker.statuses.map(x => x._1 -> x._2._1).mkString(",")}")
    if ((firstCondition || secondCondition) && thirdCondition) {
      val requestedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)

      //logger.info(s"Awaiting for: ${requestedModifiersFromPeer.keys.map(elem => Algos.encode(elem.toArray)).mkString(",")}")
      //logger.info(s"Trying to make request for: ${modifierIds.map(Algos.encode).mkString(",")}")

      //val receivedModifiersByPeer: Set[ModifierIdAsKey] = receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)

      val notYetRequested: Seq[ModifierId] = modifierIds.filter(id => !history.contains(id)
        && !requestedModifiersFromPeer.contains(toKey(id)) && !receivedModifiers.contains(toKey(id)))

      //      val notYetRequested: Seq[ModifierId] = modifierIds.filterNot(id => (history.contains(id)
      //        || /*!receivedModifiersByPeer.contains(toKey(id))*/ requestedModifiersFromPeer.contains(toKey(id))))

      //logger.info(s"After filter: ${notYetRequested.map(Algos.encode).mkString(",")}")

      if (notYetRequested.nonEmpty) {
        logger.info(s"Send request to ${peer.socketAddress.getAddress} for modifiers of type $mTypeId " +
          s"with ${notYetRequested.size}|${notYetRequested.map(Algos.encode).mkString(",")} ids.")
        peer.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> notYetRequested)
        syncTracker.incrementRequestForNModifiers(peer, notYetRequested.size)
        logger.info(s"PEER -> $peer")
        //        logger.info(s"${requestedModifiersFromPeer.map(x => Algos.encode(x._1.toArray)).mkString(",")} - expected mods by $peer")
        val requestedModIds: Map[ModifierIdAsKey, (Cancellable, PeerPriorityStatus)] =
          notYetRequested.foldLeft(requestedModifiersFromPeer) { case (rYet, id) =>
            rYet.updated(
              toKey(id),
              context.system
                .scheduler.scheduleOnce(settings.network.deliveryTimeout)(schedulerChecker(peer, mTypeId, id)) -> 1)
          }
        logger.info(s"UPDATED ${requestedModIds.map(x => Algos.encode(x._1.toArray)).mkString(",")} - expected mods by $peer")
        expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, requestedModIds)
      }
    }
  }

  def schedulerChecker(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    logger.info(s"\n\n TRIGGERED SCHEDULER FOR ${Algos.encode(modifierId)} \n\n")
    checkDelivery(peer, modifierTypeId, modifierId)
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
          innerPeerAddr == peer.socketAddress.getAddress &&
            cResult != Younger && cResult != Fork
        } match {
          case Some((_, (_, _, cP))) =>
            cP.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> Seq(modId))
            logger.info(s"Re-asked ${peer.socketAddress} and handler: ${peer.handlerRef} for modifier of type: " +
              s"$mTypeId with id: ${Algos.encode(modId)}. Attempts: $attempts")
            syncTracker.incrementRequest(peer)
            expectedModifiers = expectedModifiers.updated(peer.socketAddress.getAddress, peerRequests.updated(
              toKey(modId),
              context.system.scheduler
                .scheduleOnce(settings.network.deliveryTimeout)(schedulerChecker(peer, mTypeId, modId)) -> (attempts + 1)
            ))
          case None =>
            expectedModifiers = clearExpectedModifiersCollection(peerRequests, toKey(modId), peer.socketAddress.getAddress)
            logger.info(s"Tried to re-ask modifier ${Algos.encode(modId)}, but this id not needed from this peer")
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
  def isExpecting(mId: ModifierId, peer: ConnectedPeer): Boolean = {
    //    logger.info("||===>>>>>> isExpecting <<<<<=======|||")
    //    logger.info(s"isExpecting -> -> ${Algos.encode(mId)}")
    //    logger.info(s"isExpecting -> ${expectedModifiers.map(x => x._1 -> x._2.map(k => Algos.encode(k._1.toArray))).mkString(",")} ")
    //    logger.info(s"isExpecting -> ${expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty).map(x => Algos.encode(x._1.toArray))}")
    //    logger.info(s"isExpecting -> ${expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty).contains(toKey(mId))}")
    //    logger.info("|||===>>>>>> isExpecting <<<<<=======|||")
    expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty).contains(toKey(mId))
  }

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
      val (withBadNodes, withoutBadNodes) = syncTracker.getPeersForConnection.partition {
        case (_, (_, priority, _)) => priority == SyncTracker.PeerPriorityStatus.BadNode
      }
      val resultedPeerCollection: Vector[(InetAddress, (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer))] =
        if (withBadNodes.nonEmpty) withoutBadNodes :+ Random.shuffle(withBadNodes).head
        else withoutBadNodes
      logger.info(s"Blockchain is not synced. acceptedPeers: $resultedPeerCollection")
      if (resultedPeerCollection.nonEmpty) {
        val shuffle = Random.shuffle(resultedPeerCollection)
        val cP = shuffle.last._2._3
        logger.info(s"shuffle: $shuffle")
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        logger.info(s"requestModifies for peer ${cP.socketAddress.getAddress} for mods: ${modifierIds.map(Algos.encode).mkString(",")}")
        requestModifies(history, cP, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
      } else logger.info(s"BlockChain is not synced. There is no nodes, which we can connect with.")
    }
    else syncTracker.getPeersForConnection match {
      case coll: Vector[_] if coll.nonEmpty =>
        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
        coll.foreach { case (_, (_, _, cP)) =>
          logger.info(s"Sent download request to the ${cP.socketAddress}.")
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
      logger.info(s"${isExpecting(mId, peer)}")
      //logger.info()
      logger.info(s"Got new modifier with type $mTid from: ${peer.socketAddress}. with id ${Algos.encode(mId)}")
      syncTracker.incrementReceive(peer)
      val peerExpectedModifiers: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
        .getOrElse(peer.socketAddress.getAddress, Map.empty)
      peerExpectedModifiers.get(toKey(mId)).foreach(_._1.cancel())
      if (mTid != Transaction.ModifierTypeId) receivedModifiers += toKey(mId)
      //logger.info(s"${peerExpectedModifiers.map(x => Algos.encode(x._1.toArray)).mkString(",")} - expected mods by peer BEFORE REMOVING")
      expectedModifiers = clearExpectedModifiersCollection(peerExpectedModifiers, toKey(mId), peer.socketAddress.getAddress)
      //logger.info(s"${peerExpectedModifiers.map(x => Algos.encode(x._1.toArray)).mkString(",")} - expected mods by peer AFTER REMOVING")
      if (isBlockChainSynced && mTid == Header.modifierTypeId) {
        logger.info(s"Received header with id: ${Algos.encode(mId)} from peer: ${peer.socketAddress.getAddress}")
        headersForPriorityRequest = headersForPriorityRequest
          .updated(toKey(mId), headersForPriorityRequest.getOrElse(toKey(mId), Seq.empty) :+ peer.socketAddress.getAddress)
        //        logger.info(s"After updating headersForPriorityRequest contains ${headersForPriorityRequest.get(toKey(mId))}")
      }
    } else {
      receivedSpamModifiers = receivedSpamModifiers - toKey(mId) + (toKey(mId) -> peer)
      syncTracker.decrementRequest(peer)
    }

  /**
    * Transform modifier id to WrappedArray.ofBytes
    *
    * @param id - modifier id which will be transform to WrappedArray of bytes
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
    * @return - expectedModifiers collection without 'peer' or expectedModifiers with updated 'peer' expected collection.
    */
  def clearExpectedModifiersCollection(expectedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)],
                                       modifierId: ModifierIdAsKey,
                                       peer: InetAddress): Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = {
    logger.info(s"===>>>clearExpectedModifiersCollection<<<===")
    //    logger.info(s"Trigger clearCollection with peer = $peer and modifier = ${Algos.encode(modifierId.toArray)}")
    //    logger.info(s"Received collection from peer is: ${expectedModifiersFromPeer.map(x => Algos.encode(x._1.toArray))}")
    val collectionWithoutModId: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiersFromPeer - modifierId
    //    logger.info(s"Colelction after removing => ${collectionWithoutModId.map(x => Algos.encode(x._1.toArray))}")
    val a: Map[InetAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = collectionWithoutModId match {
      case coll: Map[_, _] if coll.nonEmpty => expectedModifiers.updated(peer, coll)
      case _ => expectedModifiers - peer
    }
    //    logger.info(s"${a.map(x => x._1 -> x._2.map(k => Algos.encode(k._1.toArray))).mkString(",")}")
    logger.info(s"===>>>clearExpectedModifiersCollection<<<===")
    a
  }
}

object DeliveryManager {

  case object FullBlockChainIsSynced

  case class CheckModifiersWithQueueSize(size: Int)

  case class ModifiersFromNVH(mods: Seq[(ModifierTypeId, ModifierId)])

  def props(influxRef: Option[ActorRef],
            nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: EncryAppSettings): Props =
    Props(new DeliveryManager(influxRef, nodeViewHolderRef, networkControllerRef, settings))
}