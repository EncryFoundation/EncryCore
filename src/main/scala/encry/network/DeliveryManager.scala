package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, PoisonPill, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus._
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.DeliveryManager.{CheckPayloadsToDownload, _}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest, SerializedModifierFromNetwork}
import encry.view.history.History
import encry.settings.EncryAppSettings

import scala.concurrent.duration._
import scala.collection.immutable.HashSet
import scala.collection.{IndexedSeq, mutable}
import scala.util.Random
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import encry.network.DownloadedModifiersValidator.{ModifiersForValidating}
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.PeersKeeper.ConnectionStatusMessages.ConnectionStopped
import encry.network.PeersKeeper._
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus.BadNode
import encry.view.mempool.MemoryPool.{StartTransactionsValidation, StopTransactionsValidation}
import org.encryfoundation.common.modifiers.history.{Block, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

import scala.concurrent.ExecutionContextExecutor
//
//class DeliveryManager(influxRef: Option[ActorRef],
//                      nodeViewHolderRef: ActorRef,
//                      networkControllerRef: ActorRef,
//                      memoryPoolRef: ActorRef,
//                      nodeViewSync: ActorRef,
//                      downloadedModifiersValidator: ActorRef,
//                      settings: EncryAppSettings) extends Actor with StrictLogging {
//
//  type ModifierIdAsKey = scala.collection.mutable.WrappedArray.ofByte
//
//  implicit val exCon: ExecutionContextExecutor = context.dispatcher
//
//  /**
//    * Collection with spam modifiers.
//    * Modifier considered spam if we receive it but it doesn't contain in expected modifiers collection.
//    */
//  var receivedSpamModifiers: Map[ModifierIdAsKey, ConnectedPeer] = Map.empty
//  /**
//    * Collection of received modifiers ids.
//    * Modifier considered received if we sent request for it and received it in special period.
//    */
//  var receivedModifiers: HashSet[ModifierIdAsKey] = HashSet.empty[ModifierIdAsKey]
//  /**
//    * Collection of expected modifiers ids.
//    * Modifier considered expected if we sent request for it.
//    */
//  var expectedModifiers: Map[InetSocketAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = Map.empty
//
//  var expectedTransactions: HashSet[ModifierIdAsKey] = HashSet.empty[ModifierIdAsKey]
//
//  var peersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryComparisonResult, PeersPriorityStatus)] = Map.empty
//
//  var priorityCalculator: PrioritiesCalculator = PrioritiesCalculator(settings.network)
//
//  var canProcessTransactions: Boolean = true
//
//  override def preStart(): Unit = {
//    networkControllerRef ! RegisterMessagesHandler(
//      Seq(ModifiersNetworkMessage.NetworkMessageTypeID -> "ModifiersNetworkMessage"), self)
//    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
//  }
//
//  override def receive: Receive = {
//    case UpdatedHistory(historyReader) =>
//      logger.debug(s"Got message with history. Starting normal actor's work.")
//      context.system.scheduler.schedule(0.second, priorityCalculator.updatingStatisticTime) {
//        val (accumulatedStatistic: Map[InetSocketAddress, PeersPriorityStatus], newStat: PrioritiesCalculator) =
//          priorityCalculator.accumulatePeersStatistic
//        priorityCalculator = newStat
//        context.parent ! AccumulatedPeersStatistic(accumulatedStatistic)
//      }
//      val checkModsSch = context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(
//        self ! CheckPayloadsToDownload
//      )
//      nodeViewSync ! SendLocalSyncInfo
//      context.become(basicMessageHandler(historyReader, isBlockChainSynced = false, isMining = settings.node.mining, checkModsSch))
//    case message => logger.debug(s"Got new message $message while awaiting history.")
//  }
//
//  def basicMessageHandler(history: History,
//                          isBlockChainSynced: Boolean,
//                          isMining: Boolean,
//                          checkModScheduler: Cancellable): Receive = {
//    case InvalidModifier(id) => receivedModifiers -= toKey(id)
//
//    case CheckDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
//      checkDelivery(peer, modifierTypeId, modifierId)
//
//    case UpdatedPeersCollection(newPeers) =>
//      logger.info(s"Delivery manager got updated peers collection.")
//      peersCollection = newPeers
//
//    case ConnectionStopped(peer) =>
//      peersCollection -= peer
//      logger.info(s"Removed peer: $peer from peers collection on Delivery Manager." +
//        s" Current peers are: ${peersCollection.mkString(",")}")
//
//    case OtherNodeSyncingStatus(remote, status, extOpt) =>
//      status match {
//        case Unknown => logger.info("Peer status is still unknown.")
//        case Younger | Fork if isBlockChainSynced => sendInvData(remote, status, extOpt)
//        case _ =>
//      }
//
//    case CheckPayloadsToDownload =>
//      val currentQueue: HashSet[ModifierIdAsKey] =
//        expectedModifiers.flatMap { case (_, modIds) => modIds.keys }.to[HashSet]
//      logger.debug(s"Current queue: ${currentQueue.map(elem => Algos.encode(elem.toArray)).mkString(",")}")
//      logger.debug(s"receivedModifiers: ${receivedModifiers.map(id => Algos.encode(id.toArray)).mkString(",")}")
//      logger.debug(s"Qty to req: ${settings.network.networkChunkSize - currentQueue.size - receivedModifiers.size}")
//      logger.debug(s"currentQueue.size: ${currentQueue.size}")
//      logger.debug(s"receivedModifiers.size: ${receivedModifiers.size}")
//      val newIds: Seq[ModifierId] =
//        history.payloadsIdsToDownload(
//          settings.network.networkChunkSize - currentQueue.size - receivedModifiers.size,
//          currentQueue.map(elem => ModifierId @@ elem.toArray)
//        ).filterNot(modId => currentQueue.contains(toKey(modId)) || receivedModifiers.contains(toKey(modId)))
//      logger.debug(s"newIds: ${newIds.map(elem => Algos.encode(elem)).mkString(",")}")
//      if (newIds.nonEmpty) requestDownload(Payload.modifierTypeId, newIds, history, isBlockChainSynced, isMining)
//      val nextCheckModsScheduler =
//        context.system.scheduler.scheduleOnce(settings.network.modifierDeliverTimeCheck)(self ! CheckPayloadsToDownload)
//      context.become(basicMessageHandler(history, isBlockChainSynced, settings.node.mining, nextCheckModsScheduler))
//
//    case SemanticallySuccessfulModifier(mod) =>
//      logger.info(s"Got SemanticallySuccessfulModifier with id: ${Algos.encode(mod.id)} of type ${mod.modifierTypeId} on dm")
//      mod match {
//        case block: Block => receivedModifiers -= toKey(block.payload.id)
//        case _ => receivedModifiers -= toKey(mod.id)
//      }
//      if (!isBlockChainSynced && expectedModifiers.isEmpty && receivedModifiers.isEmpty) {
//        checkModScheduler.cancel()
//        logger.debug(s"SemanticallySuccessfulModifier case, if condition true. Resend CheckPayloadsToDownload to DM")
//        self ! CheckPayloadsToDownload
//      }
//    case SemanticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
//
//    case SyntacticallyFailedModification(mod, _) => receivedModifiers -= toKey(mod.id)
//
//    case SuccessfulTransaction(_) => //do nothing
//
//    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
//      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got RequestFromLocal on NVSH from $sender with " +
//        s"ids of type: $modifierTypeId. Number of ids is: ${modifierIds.size}. Ids: ${modifierIds.map(Algos.encode).mkString(",")}. Sending request from local to DeliveryManager.")
//      if (modifierIds.nonEmpty) requestModifies(history, peer, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
//
//    case DataFromPeer(message, remote) => message match {
//      case ModifiersNetworkMessage((typeId, modifiers)) =>
//        logger.debug(s"Received modifiers are: ${modifiers.map(x => Algos.encode(x._1)).mkString(",")}")
//        influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
//        for ((id, _) <- modifiers) receive(typeId, id, remote, isBlockChainSynced)
//        val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) = modifiers.partition(p => isSpam(p._1))
//        if (spam.nonEmpty) {
//          if (typeId != Transaction.modifierTypeId)
//            logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
//              s": ${spam.keys.map(Algos.encode)}.")
//          receivedSpamModifiers = Map.empty
//        }
//        val filteredModifiers: Map[ModifierId, Array[Byte]] = fm.filterKeys(k => !history.isModifierDefined(k))
//        if (typeId != Transaction.modifierTypeId) influxRef
//          .foreach(ref => (0 to filteredModifiers.size).foreach(_ => ref ! SerializedModifierFromNetwork(typeId)))
//        //todo check this logic
//        logger.debug(s"Type of mod: ${typeId}. canProcessTransactions: ${canProcessTransactions}")
//        if ((typeId == Transaction.modifierTypeId && canProcessTransactions) || (typeId != Transaction.modifierTypeId))
//          downloadedModifiersValidator ! ModifiersForValidating(remote, typeId, filteredModifiers)
//
//      case _ => logger.debug(s"DeliveryManager got invalid type of DataFromPeer message!")
//    }
//
//    case DownloadRequest(modifierTypeId, modifiersId, previousModifier) =>  //todo check this condition
//      if (modifierTypeId != Transaction.modifierTypeId)
//        logger.info(s"DownloadRequest for mod ${Algos.encode(modifiersId)} of type: $modifierTypeId prev mod: " +
//          s"${previousModifier.map(Algos.encode)}")
//      requestDownload(modifierTypeId, Seq(modifiersId), history, isBlockChainSynced, isMining)
//
//    case PeersForSyncInfo(peers) => sendSync(history.syncInfo, peers)
//
//    case FullBlockChainIsSynced => context.become(basicMessageHandler(history, isBlockChainSynced = true, isMining, checkModScheduler))
//
//    case StartMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = true, checkModScheduler))
//
//    case DisableMining => context.become(basicMessageHandler(history, isBlockChainSynced, isMining = false, checkModScheduler))
//
//    case UpdatedHistory(historyReader) => context.become(basicMessageHandler(historyReader, isBlockChainSynced, isMining, checkModScheduler))
//
//    case StopTransactionsValidation => canProcessTransactions = false
//
//    case StartTransactionsValidation => canProcessTransactions = true
//
//    case message => logger.debug(s"Got strange message $message(${message.getClass}) on DeliveryManager from $sender")
//  }
//
//  /**
//    * This function check if modifier has received or not.
//    * If modifier has transaction type id, it won't be re-asked.
//    * If we still have no this modifier and number of attempts have no expired, we will re-ask it.
//    * If we still have no this modifier and number of attempts have expired, we will remove it from expected modifiers collection.
//    * Otherwise - do nothing.
//    *
//    * @param peer           - peer, from whom we are expecting modifier
//    * @param modifierTypeId - type of expected modifier
//    * @param modifierId     - expected modifier id
//    */
//  def checkDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
//    val expectedModifiersByPeer: Map[ModifierIdAsKey, (Cancellable, Int)] =
//      expectedModifiers.getOrElse(peer.socketAddress, Map.empty)
//    if (modifierTypeId == Transaction.modifierTypeId)
//      expectedModifiers = clearExpectedModifiersCollection(expectedModifiersByPeer, toKey(modifierId), peer.socketAddress)
//    else expectedModifiersByPeer.find { case (id, (_, _)) => id == toKey(modifierId) } match {
//      case Some((_, (_, attempts))) if attempts <= settings.network.maxDeliveryChecks =>
//        logger.debug(s"Modifier ${Algos.encode(modifierId)} needed to be requested from $peer!")
//        reRequestModifier(peer, modifierTypeId, modifierId, expectedModifiersByPeer)
//      case Some((modId, (_, _))) =>
//        logger.debug(s"Maximum number of attempts has expired. Remove modifier ${Algos.encode(modifierId)} from $peer.")
//        expectedModifiers = clearExpectedModifiersCollection(expectedModifiersByPeer, modId, peer.socketAddress)
//      case _ =>
//        logger.debug(s"This modifiers ${Algos.encode(modifierId)} is not contained in expectedModifiers collection from $peer.")
//    }
//  }
//
//  /**
//    * If node is not synced, send sync info to random peer, otherwise to all known peers.
//    *
//    * @param syncInfo - sync info
//    */
//  def sendSync(syncInfo: SyncInfo, peers: Seq[ConnectedPeer]): Unit = peers.foreach { peer =>
//    logger.info(s"Sending to $peer sync info message.")
//    peer.handlerRef ! SyncInfoNetworkMessage(syncInfo)
//  }
//
//  /**
//    * Send request to 'peer' with modifiers ids of type 'modifierTypeId'.
//    * We can do this activity only if 'peer' status != Younger.
//    * If current chain isn't synced and mining is off, we can't request transactions, otherwise can.
//    *
//    * We should filter our requesting modifiers to avoid request repeated modifiers.
//    *
//    * @param history            - current history reader
//    * @param peer               - peer, whom message will be send
//    * @param mTypeId            - modifier type id
//    * @param modifierIds        - modifiers ids
//    * @param isBlockChainSynced - current block chain status
//    * @param isMining           - current mining status
//    */
//
//  def requestModifies(history: History,
//                      peer: ConnectedPeer,
//                      mTypeId: ModifierTypeId,
//                      modifierIds: Seq[ModifierId],
//                      isBlockChainSynced: Boolean,
//                      isMining: Boolean): Unit = {
//    val firstCondition: Boolean = mTypeId == Transaction.modifierTypeId && isBlockChainSynced && isMining
//    val secondCondition: Boolean = mTypeId != Transaction.modifierTypeId
//    val thirdCondition: Boolean =
//      if (!isBlockChainSynced) peersCollection.get(peer.socketAddress).exists(p => p._2 != Younger)
//      else peersCollection.contains(peer.socketAddress)
//    if (mTypeId != Transaction.modifierTypeId)
//      logger.debug(s"Got requestModifier for modifiers of type: $mTypeId to $peer with modifiers ${modifierIds.size}." +
//        s" Try to check conditions: $firstCondition -> $secondCondition -> $thirdCondition.")
//    if ((firstCondition || secondCondition) && thirdCondition) {
//      val requestedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
//        .getOrElse(peer.socketAddress, Map.empty)
//
//      val notYetRequested: Seq[ModifierId] = modifierIds
//        .filter(id =>
//          !history.isModifierDefined(id) &&
//            !requestedModifiersFromPeer.contains(toKey(id)) &&
//            !receivedModifiers.contains(toKey(id))
//        )
//
//      if (notYetRequested.nonEmpty) {
//        if (mTypeId != Transaction.modifierTypeId)
//          logger.debug(s"Send request to ${peer.socketAddress} for ${notYetRequested.size} modifiers of type $mTypeId ")
//        peer.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> notYetRequested)
//        priorityCalculator = priorityCalculator.incrementRequestForNModifiers(peer.socketAddress, notYetRequested.size)
//        if (mTypeId != Transaction.modifierTypeId) {
//          val requestedModIds: Map[ModifierIdAsKey, (Cancellable, Int)] =
//            notYetRequested.foldLeft(requestedModifiersFromPeer) { case (rYet, id) =>
//              rYet.updated(toKey(id),
//                context.system
//                  .scheduler.scheduleOnce(settings.network.deliveryTimeout)(self ! CheckDelivery(peer, mTypeId, id)) -> 1)
//            }
//          expectedModifiers = expectedModifiers.updated(peer.socketAddress, requestedModIds)
//        } else expectedTransactions = expectedTransactions ++ modifierIds.map(toKey)
//      }
//    }
//  }
//
//  /**
//    * Re-ask 'modifierId' from 'peer' if needed. We will do this only if we are expecting these modifier from 'peer'
//    * and if number of attempts doesn't expired yet.
//    * This activity will update timer on re-asked modifier.
//    *
//    * @param peer    - peer, whom message will be send
//    * @param mTypeId - modifier type id
//    * @param modId   - re-asked modifier id
//    */
//  def reRequestModifier(peer: ConnectedPeer,
//                        mTypeId: ModifierTypeId,
//                        modId: ModifierId,
//                        peerRequests: Map[ModifierIdAsKey, (Cancellable, Int)]): Unit =
//    peerRequests.get(toKey(modId)) match {
//      case Some((_, attempts)) => peersCollection.find { case (innerAddr, (_, cResult, _)) =>
//        innerAddr == peer.socketAddress && cResult != Younger
//      } match {
//        case Some((_, (cP, _, _))) =>
//          cP.handlerRef ! RequestModifiersNetworkMessage(mTypeId -> Seq(modId))
//          logger.debug(s"Re-asked ${peer.socketAddress} and handler: ${peer.handlerRef} for modifier of type: " +
//            s"$mTypeId with id: ${Algos.encode(modId)}. Attempts: $attempts")
//          priorityCalculator = priorityCalculator.incrementRequest(peer.socketAddress)
//          expectedModifiers = expectedModifiers.updated(peer.socketAddress, peerRequests.updated(
//            toKey(modId),
//            context.system.scheduler
//              .scheduleOnce(settings.network.deliveryTimeout)(self ! CheckDelivery(peer, mTypeId, modId)) -> (attempts + 1)
//          ))
//        case None =>
//          expectedModifiers = clearExpectedModifiersCollection(peerRequests, toKey(modId), peer.socketAddress)
//          logger.debug(s"Tried to re-ask modifier ${Algos.encode(modId)}, but this id not needed from this peer")
//      }
//      case _ => logger.debug(s"There is no such modifier ${Algos.encode(modId)} in expected collection.")
//    }
//
//  /**
//    * Check 'expectedModifiers' for awaiting modifier with id 'mId' from 'peer'
//    *
//    * @param mId  - id of checkable modifier
//    * @param peer - peer from which we possibly expecting modifier
//    * @return 'true' if we are expecting this modifier from this peer otherwise 'false'
//    */
//  def isExpecting(mId: ModifierId, modifierTypeId: ModifierTypeId, peer: ConnectedPeer): Boolean = {
//    if (modifierTypeId != Transaction.modifierTypeId) {
//      val result: Boolean = expectedModifiers.getOrElse(peer.socketAddress, Map.empty).contains(toKey(mId))
//      logger.debug(s"isExpecting -->> modId ${Algos.encode(mId)} --> $result")
//      result
//    } else expectedTransactions.contains(toKey(mId))
//  }
//
//  /**
//    * Clear the 'receivedSpamModifiers' collection
//    *
//    * @param mIds - sequence of modifiers ids which will be deleted from spam collection
//    */
//  def deleteSpam(mIds: Seq[ModifierId]): Unit = for (id <- mIds) receivedSpamModifiers -= toKey(id)
//
//  /**
//    * Check receivedSpamModifiers for contains received modifier
//    *
//    * @param mId - checkable modifier
//    * @return 'true' if received modifier is in spam collection otherwise 'false'
//    */
//  def isSpam(mId: ModifierId): Boolean = receivedSpamModifiers.contains(toKey(mId))
//
//  /**
//    * Send inv data to the 'peer'.
//    *
//    * @param peer              - peer whom will send a message
//    * @param status            - current peer's status
//    * @param dataForInvMessage - data for inv message
//    */
//  def sendInvData(peer: ConnectedPeer,
//                  status: HistoryComparisonResult,
//                  dataForInvMessage: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = dataForInvMessage match {
//    case Some(data) =>
//      data.groupBy(_._1).mapValues(_.map(_._2)).foreach {
//        case (mTid, mods) if mods.size <= settings.network.maxInvObjects =>
//          logger.debug(s"Send to peer $peer inv msg with mods: ${mods.map(Algos.encode).mkString(",")}")
//          peer.handlerRef ! InvNetworkMessage(mTid -> mods)
//        case (mTid, mods) =>
//          val modifiers: Seq[ModifierId] = mods.take(settings.network.maxInvObjects)
//          logger.debug(s"Send to peer $peer dropped inv msg with mods: ${modifiers.map(Algos.encode).mkString(",")}")
//          peer.handlerRef ! InvNetworkMessage(mTid -> modifiers)
//      }
//    case None => logger.info(s"dataForInvMessage is empty for: $peer. Peer's status is: $status.")
//  }
//
//  /**
//    * If node is not synced, `requestDownload` sends request for the one peer which will be find by 2 criteria:
//    * 1) HistoryComparisonResult != Younger.
//    * 2) Choose random peer with non bad priority.
//    * Otherwise this function sends requests for all known peers selected by 1-st criterion as above.
//    *
//    * If there are no any peers, request won't be sent.
//    *
//    * @param modifierTypeId     - modifier type id
//    * @param modifierIds        - modifier id
//    * @param history            - current history state
//    * @param isBlockChainSynced - current block chain status
//    * @param isMining           - current mining status
//    */
//  def requestDownload(modifierTypeId: ModifierTypeId,
//                      modifierIds: Seq[ModifierId],
//                      history: History,
//                      isBlockChainSynced: Boolean,
//                      isMining: Boolean): Unit =
//    if (!isBlockChainSynced) {
//      logger.debug(s"requestDownload -> !isBlockChainSynced = true")
//      val (withBadNodesMap, withoutBadNodesMap) = peersCollection.filter(p => p._2._2 != Younger).partition {
//        case (_, (_, _, priority)) => priority == BadNode
//      }
//      logger.debug(s"withBadNodesMap -> ${withBadNodesMap.keys.mkString(",")}")
//      logger.debug(s"withoutBadNodesMap -> ${withoutBadNodesMap.keys.mkString(",")}")
//      val withBadNodes: IndexedSeq[(ConnectedPeer, HistoryComparisonResult)] =
//        withBadNodesMap.map(x => x._2._1 -> x._2._2).toIndexedSeq
//      val withoutBadNodes: IndexedSeq[(ConnectedPeer, HistoryComparisonResult)] =
//        withoutBadNodesMap.map(x => x._2._1 -> x._2._2).toIndexedSeq
//      val resultedPeerCollection =
//        if (withBadNodes.nonEmpty) withoutBadNodes :+ Random.shuffle(withBadNodes).head
//        else withoutBadNodes
//      logger.debug(s"resultedPeerCollection -> $resultedPeerCollection")
//      logger.debug(s"Block chain is not synced. acceptedPeers: $resultedPeerCollection")
//      if (resultedPeerCollection.nonEmpty) {
//        val shuffle: IndexedSeq[(ConnectedPeer, HistoryComparisonResult)] = Random.shuffle(resultedPeerCollection)
//        val cP = shuffle.last._1
//        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
//        if (modifierTypeId != Transaction.modifierTypeId)
//          logger.debug(s"requestModifies for peer ${cP.socketAddress} for mods: ${modifierIds.map(Algos.encode).mkString(",")}")
//        requestModifies(history, cP, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
//      } else logger.info(s"BlockChain is not synced. There is no nodes, which we can connect with.")
//    }
//    else peersCollection.filter(p => p._2._2 != Younger) match {
//      case coll: Map[_, _] if coll.nonEmpty =>
//        influxRef.foreach(_ ! SendDownloadRequest(modifierTypeId, modifierIds))
//        coll.foreach { case (_, (cp, _, _)) =>
//          if (modifierTypeId != Transaction.modifierTypeId)
//            logger.info(s"Sent download request to the ${cp.socketAddress} to modifiers of type: $modifierTypeId.")
//          requestModifies(history, cp, modifierTypeId, modifierIds, isBlockChainSynced, isMining)
//        }
//      case _ => logger.info(s"BlockChain is synced. There is no nodes, which we can connect with.")
//    }
//
//  /**
//    * Handle received modifier. We will process received modifier only if we are expecting this on.
//    *
//    * @param mTid               - modifier type id
//    * @param mId                - modifier id
//    * @param peer               - peer who sent modifier
//    * @param isBlockChainSynced - current chain status
//    */
//  def receive(mTid: ModifierTypeId,
//              mId: ModifierId,
//              peer: ConnectedPeer,
//              isBlockChainSynced: Boolean): Unit =
//    if (isExpecting(mId, mTid, peer)) {
//      if (mTid != Transaction.modifierTypeId) {
//        logger.debug(s"Got new modifier with type $mTid from: ${peer.socketAddress}. with id ${Algos.encode(mId)}")
//      }
//      priorityCalculator = priorityCalculator.incrementReceive(peer.socketAddress)
//      val peerExpectedModifiers: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiers
//        .getOrElse(peer.socketAddress, Map.empty)
//      peerExpectedModifiers.get(toKey(mId)).foreach(_._1.cancel())
//      if (mTid != Transaction.modifierTypeId) receivedModifiers += toKey(mId)
//      if (mTid != Transaction.modifierTypeId) expectedModifiers = clearExpectedModifiersCollection(peerExpectedModifiers, toKey(mId), peer.socketAddress)
//      else expectedTransactions = expectedTransactions - toKey(mId)
//    } else {
//      receivedSpamModifiers = receivedSpamModifiers - toKey(mId) + (toKey(mId) -> peer)
//      priorityCalculator = priorityCalculator.decrementRequest(peer.socketAddress)
//    }
//
//  /**
//    * Transform modifier id to WrappedArray.ofBytes
//    *
//    * @param id - modifier id which will be transform to WrappedArray of bytes.
//    * @return transformed modifier id
//    */
//  def toKey(id: ModifierId): ModifierIdAsKey = new mutable.WrappedArray.ofByte(id)
//
//  /**
//    * This function gets collection of current expected modifiers from 'peer' and modifier, which
//    * will be removed from received collection as a parameters.
//    * If expected modifiers collection will contain other modifiers even after removing,
//    * this function will return collection of expectedModifiers with updated 'peer' expected collection
//    * otherwise it will return expectedModifiers collection without 'peer'.
//    *
//    * @param expectedModifiersFromPeer - collection of expected modifiers from 'peer'
//    * @param modifierId                - modifier id, which will be removed from 'expectedModifiersFromPeer'
//    * @param peer                      - 'peer' from which expected modifiers collection we remove received modifier
//    * @return - expectedModifiers collection without 'peer' or expectedModifiers with updated 'peer' expected collection
//    */
//  def clearExpectedModifiersCollection(expectedModifiersFromPeer: Map[ModifierIdAsKey, (Cancellable, Int)],
//                                       modifierId: ModifierIdAsKey,
//                                       peer: InetSocketAddress): Map[InetSocketAddress, Map[ModifierIdAsKey, (Cancellable, Int)]] = {
//    val collectionWithoutModId: Map[ModifierIdAsKey, (Cancellable, Int)] = expectedModifiersFromPeer - modifierId
//    collectionWithoutModId match {
//      case coll: Map[_, _] if coll.nonEmpty => expectedModifiers.updated(peer, coll)
//      case _ => expectedModifiers - peer
//    }
//  }
//}
//
object DeliveryManager {

  final case class CheckDelivery(peer: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierId: ModifierId)

  case object CheckPayloadsToDownload

  final case object FullBlockChainIsSynced

  final case class CheckModifiersWithQueueSize(size: Int) extends AnyVal

//  def props(influxRef: Option[ActorRef],
//            nodeViewHolderRef: ActorRef,
//            networkControllerRef: ActorRef,
//            memoryPoolRef: ActorRef,
//            nodeViewSync: ActorRef,
//            downloadedModifiersValidator: ActorRef,
//            settings: EncryAppSettings): Props =
//    Props(new DeliveryManager(influxRef, nodeViewHolderRef, networkControllerRef, memoryPoolRef, nodeViewSync,
//      downloadedModifiersValidator, settings))

  class DeliveryManagerPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case RequestFromLocal(_, Transaction.modifierTypeId, _) => 3

        case RequestFromLocal(_, _, _) => 0

        case StopTransactionsValidation => 2

        case StartTransactionsValidation => 2

        case OtherNodeSyncingStatus(_, _, _) => 1

        case ConnectionStopped(_) => 1


//        case DataFromPeer(msg: ModifiersNetworkMessage, _) =>
//          msg match {
//            case ModifiersNetworkMessage((typeId, _)) if typeId != Transaction.modifierTypeId => 1
//            case _ => 3
//          }

        case PoisonPill => 4

        case _ => 2
      })

}