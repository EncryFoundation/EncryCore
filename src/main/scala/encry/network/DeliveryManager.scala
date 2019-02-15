package encry.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Cancellable}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp._
import encry.consensus.History.{HistoryComparisonResult, Unknown, Younger}
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history.Header
import encry.modifiers.mempool.Transaction
import encry.network.DeliveryManager._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler._
import encry.network.SyncTracker._
import encry.network.message.BasicMsgDataTypes.ModifiersData
import encry.network.message._
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.Mempool
import org.encryfoundation.common.Algos

import scala.concurrent.duration._
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class DeliveryManager(influxRef: Option[ActorRef]) extends Actor with StrictLogging {

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

  /**
    * This collection contains statistic about network communication nodes from network with current.
    *
    * Key - peer address. Node address which we connecting with.
    *
    * Value - tuple(Requested, Received).
    * Value shows, how many modifiers has been requested and received for the current period.
    */

  private type Requested = Int
  private type Received = Int
  private var peersNetworkCommunication: Map[ConnectedPeer, (Requested, Received)] = Map.empty

  system.scheduler.schedule(
    settings.network.updatePriorityTime.seconds,
    settings.network.updatePriorityTime.seconds
  )(updatePeersPriorityStatus())

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
      val requestReceiveStat: (Requested, Received) = peersNetworkCommunication.getOrElse(peer, (0, 0))
      if (delivered.contains(key(modifierId))) {
        logger.info(s"CheckDelivery if collection before: ${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")}")
        peersNetworkCommunication = peersNetworkCommunication.updated(peer, (requestReceiveStat._1 + 1, requestReceiveStat._2 + 1))
        logger.info(s"CheckDelivery if collection after: ${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")}")
        //        logger.info(s"\nCurrent modId: ${Algos.encode(modifierId)} was delivered for the peer: ${peer.socketAddress}" +
//          s" Before peer state: $requestReceiveStat" +
//          s". Current peer state: ${peersNetworkCommunication.getOrElse(peer, (0, 0))}" +
//          s"Update stat:" +
//          s" +1 to request + 1 ti received. " +
//          s"Current peersNetworkCommunication collection statuses are: Peer, Requested -> Received" +
//          s"${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")} \n")
        delivered -= key(modifierId)
      } else {
        logger.info(s"CheckDelivery else collection before: ${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")}")
        peersNetworkCommunication = peersNetworkCommunication.updated(peer, (requestReceiveStat._1 + 1, requestReceiveStat._2))
        logger.info(s"CheckDelivery else collection after: ${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")}")
        logger.info(s"\nCurrent modId: ${Algos.encode(modifierId)} is not yet delivered." +
          s" Before peer state: $requestReceiveStat" +
          s". Current peer state: ${peersNetworkCommunication.getOrElse(peer, (0, 0))}" +
          s" Update stat:" +
          s" +1 to request + 0 ti received. " +
          s"Current peersNetworkCommunication collection statuses are: Peer, Requested -> Received" +
          s"${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")} \n")
        reexpect(peer, modifierTypeId, modifierId)
      }
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
      influxRef.foreach(_ ! GetModifiers(typeId, modifiers.keys.toSeq))
      for ((id, _) <- modifiers) receive(typeId, id, remote)
      if (spam.nonEmpty) {
        logger.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Algos.encode)}")
        deleteSpam(spam.keys.toSeq)
      }
      val filteredModifiers: Seq[Array[Byte]] = fm.filterNot { case (modId, _) => historyReaderOpt.contains(modId) }.values.toSeq
      if (filteredModifiers.nonEmpty) nodeViewHolder ! ModifiersFromRemote(typeId, filteredModifiers)
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
    * If node is not synced, send sync info to random peer, otherwise to all known peers
    *
    * @param syncInfo
    */
  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    if (isBlockChainSynced)
      statusTracker.peersToSyncWith().foreach(peer =>
        peer.handlerRef ! Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
      )
    else Random.shuffle(statusTracker.peersToSyncWith()).headOption.foreach(peer =>
      peer.handlerRef ! Message(EncrySyncInfoMessageSpec, Right(syncInfo), None)
    )
  }

  //todo: refactor
  def expect(peer: ConnectedPeer, mTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit =
    if (((mTypeId == Transaction.ModifierTypeId && isBlockChainSynced && isMining)
      || mTypeId != Transaction.ModifierTypeId) && statusTracker.statuses.get(peer).exists(_._1 != Younger)) {
      val notYetRequestedIds: Seq[ModifierId] = modifierIds.foldLeft(Vector[ModifierId]()) {
        case (notYetRequested, modId) =>
          if (historyReaderOpt.forall(history => !history.contains(modId) && !delivered.contains(key(modId)))) {
            notYetRequested :+ modId
          } else notYetRequested
      }
      if (notYetRequestedIds.nonEmpty) {
        logger.info(s"Send request to ${peer.socketAddress.getAddress} for modifiers of type $mTypeId " +
          s"with ${modifierIds.size} ids.")
        peer.handlerRef ! Message(requestModifierSpec, Right(mTypeId -> notYetRequestedIds), None)
      }
      notYetRequestedIds.foreach { id =>
        val cancellable: Cancellable = context.system.scheduler
          .scheduleOnce(settings.network.deliveryTimeout, self, CheckDelivery(peer, mTypeId, id))
        ///TODO: If modId comes here twice, it's schedule will be always 0
        val peerMap: Map[ModifierIdAsKey, (Cancellable, Int)] = cancellables
          .getOrElse(peer.socketAddress.getAddress, Map.empty)
          .updated(key(id), cancellable -> 0)
        cancellables = cancellables.updated(peer.socketAddress.getAddress, peerMap)
        val reqAttempts: Int = requestedModifiers.getOrElse(key(id), 0) + 1
        requestedModifiers = requestedModifiers.updated(key(id), reqAttempts)
      }
    }

  //todo: refactor
  def reexpect(cp: ConnectedPeer, mTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val peerAndHistoryOpt: Option[(ConnectedPeer, (HistoryComparisonResult, PeerPriority))] =
      statusTracker.statuses.find { case (peer, (comparisonResult, _)) =>
        peer.socketAddress == cp.socketAddress && comparisonResult != Younger
      }
    cancellables.get(cp.socketAddress.getAddress) match {
      case Some(modifiersInfo) =>
        modifiersInfo.get(key(modifierId)) match {
          case Some(modifierInfo) =>
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
                val peerMap = cancellables.getOrElse(peerInfo._1.socketAddress.getAddress, Map.empty) - key(modifierId)
                cancellables = cancellables.updated(peerInfo._1.socketAddress.getAddress, peerMap)
                requestedModifiers.get(key(modifierId)).foreach { qtyOfRequests =>
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
    } else logger.info(s"Peer's $remote hisotry is younger, but node is note synces, so ignore sending extentions")

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
    *
    * @param modifierTypeId
    * @param modifierIds
    */
  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    if (settings.influxDB.isDefined)
      context.actorSelection("/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    if (!isBlockChainSynced) {
      val peer: Option[ConnectedPeer] = statusTracker.statuses
        .filter(_._2._1 != Younger).map(v => v._1 -> v._2._2).toList.collect {
        case (peer1, priority: HighPriority) => peer1 -> priority.priority
        case (peer1, priority: LowPriority) => peer1 -> priority.priority
        case (peer1, priority: BadNode) => peer1 -> priority.priority
        case (peer1, priority: InitialPriority) => peer1 -> priority.priority
      }.sortBy(_._2).headOption.map(_._1)
      peer.foreach(pI => expect(pI, modifierTypeId, modifierIds))

    }
    else statusTracker.statuses.filter(x => x._2._1 != Younger).keys.foreach(peer => expect(peer, modifierTypeId, modifierIds))
  }

  def receive(mtid: ModifierTypeId, mid: ModifierId, cp: ConnectedPeer): Unit =
    if (isExpecting(mtid, mid)) {
      val requestReceiveStat: (Requested, Received) = peersNetworkCommunication.getOrElse(cp, (0, 0))
      peersNetworkCommunication = peersNetworkCommunication.updated(cp, (requestReceiveStat._1 + 1, requestReceiveStat._2 + 1))
      //      logger.info(s"\nCurrent modId: ${Algos.encode(mid)} is RECEIVED. Update stat:" +
//        s" +1 to request + 1 ti received. " +
//        s"Current peersNetworkCommunication collection statuses are: Peer, Requested -> Received" +
//        s"${peersNetworkCommunication.map(x => (x, x._2._1 -> x._2._2)).mkString(",")} \n")
      //todo: refactor
      delivered = delivered + key(mid)
      val peerMap: Map[ModifierIdAsKey, (Cancellable, Requested)] =
        cancellables.getOrElse(cp.socketAddress.getAddress, Map.empty)
      peerMap.get(key(mid)).foreach(_._1.cancel())
      requestedModifiers = requestedModifiers - key(mid)
      val peerMapWithoutModifier: Map[ModifierIdAsKey, (Cancellable, Requested)] = peerMap - key(mid)
      cancellables = cancellables.updated(cp.socketAddress.getAddress, peerMapWithoutModifier)
      if (isBlockChainSynced && mtid == Header.modifierTypeId) {
        val peersWhoDelivered: Seq[InetAddress] = deliveredModifiersMap
          .getOrElse(key(mid), Seq.empty) :+ cp.socketAddress.getAddress
        deliveredModifiersMap = deliveredModifiersMap.updated(key(mid), peersWhoDelivered)
      }
    } else deliveredSpam = deliveredSpam - key(mid) + (key(mid) -> cp)

  def updatePeersPriorityStatus(): Unit = {
    peersNetworkCommunication.foreach { case (peer, (requested, received)) =>
      logger.info(s"peer: ${peer.socketAddress}")
      val priority: PeerPriority = received / requested match {
        case a if a > 0.66 => HighPriority()
        case a if a > 0.33 => LowPriority()
        case a => BadNode()
      }
      val currentStatus: Option[(HistoryComparisonResult, PeerPriority)] = statusTracker.statuses.get(peer)
      currentStatus match {
        case Some(v) =>
          logger.info(s"Update peers priority: peer :${peer.socketAddress} has new priority: ${priority.toString}" +
          s" instead of old: ${v._2.toString}")
          statusTracker.statuses = statusTracker.statuses.updated(peer, (v._1, priority))
          logger.info(s"StatusTracker: ${statusTracker.statuses.map(x => x._1 -> (x._2._2, x._2._1)).mkString(",")}")
        case None =>
          logger.info(s"No such peer in status tracker!")
      }
    }
    peersNetworkCommunication = Map.empty[ConnectedPeer, (Requested, Received)]
  }
}

object DeliveryManager {

  case object FullBlockChainSynced

}