package encry.network

import java.net.InetSocketAddress

import akka.actor.Actor
import encry.EncryApp._
import encry.consensus.History._
import encry.consensus.{HistoryReader, SyncInfo}
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.Transaction
import encry.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.message.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.message._
import encry.settings.NetworkSettings
import encry.stats.StatsSender.{GetModifiers, SendDownloadRequest}
import encry.utils.ScorexLogging
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges, ModifiersFromRemote}
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.{EncryMempool, MempoolReader}
import encry.view.state.{Proposition, StateReader}
import encry.{ModifierId, ModifierTypeId, VersionTag}
import scorex.crypto.encode.Base58

class EncryNodeViewSynchronizer(syncInfoSpec: EncrySyncInfoMessageSpec.type) extends Actor with ScorexLogging {


  val networkSettings: NetworkSettings = settings.network
  val deliveryTracker: EncryDeliveryTracker =
    EncryDeliveryTracker(context, networkSettings.deliveryTimeout, networkSettings.maxDeliveryChecks, self, timeProvider)
  val invSpec: InvSpec = new InvSpec(networkSettings.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  val statusTracker: SyncTracker = SyncTracker(self, context, networkSettings, timeProvider)
  var historyReaderOpt: Option[EncryHistory] = None
  var mempoolReaderOpt: Option[EncryMempool] = None

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    networkController ! SendToNetwork(Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None), Broadcast)

  def sendSync(syncInfo: EncrySyncInfo): Unit = {
    val peers: Seq[ConnectedPeer] = statusTracker.peersToSyncWith()
    if (peers.nonEmpty)
      networkController ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peers))
  }

  // Send history extension to the (less developed) peer 'remote' which does not have it.
  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => logWarn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  override def receive: Receive = viewHolderEvents orElse {
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < (networkSettings.syncInterval.toMillis / 2))
        log.info("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => logWarn("Peer status is still unknown")
        case Nonsense => logWarn("Got nonsense")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (deliveryTracker.peerWhoDelivered(modifierId).contains(peer)) deliveryTracker.delete(modifierId)
      else {
        log.info(s"Peer $peer has not delivered asked modifier ${Base58.encode(modifierId)} on time")
        deliveryTracker.reexpect(peer, modifierTypeId, modifierId)
      }
    case DataFromPeer(spec, syncInfo: EncrySyncInfo@unchecked, remote) if spec.messageCode == syncInfoSpec.messageCode =>
      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, networkSettings.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          log.info(s"Comparison with $remote having starting points ${encry.idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          log.info(s"Extension ids: ${encry.idsToString(ext)}")
          if (!(extensionOpt.nonEmpty || comparison != Younger)) logWarn("Extension is empty while comparison is younger")
          self ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
    case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == RequestModifierSpec.MessageCode =>
      historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp))).foreach { readers =>
        val objs: Seq[NodeViewModifier] = invData._1 match {
          case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => readers._2.getAll(invData._2)
          case _: ModifierTypeId => invData._2.flatMap(id => readers._1.modifierById(id))
        }
        log.info(s"Requested ${invData._2.length} modifiers ${encry.idsToString(invData)}, " +
          s"sending ${objs.length} modifiers ${encry.idsToString(invData._1, objs.map(_.id))} ")
        self ! ResponseFromLocal(remote, invData._1, objs)
      }
    case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == InvSpec.MessageCode =>
      nodeViewHolder ! CompareViews(remote, invData._1, invData._2)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      if (settings.node.sendStat)
        context.actorSelection("akka://encry/user/statsSender") ! GetModifiers(typeId, modifiers.keys.toSeq)
      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      log.info(s"Received modifier ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      for ((id, _) <- modifiers) deliveryTracker.receive(typeId, id, remote)
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => deliveryTracker.isSpam(id) }
      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Base58.encode)}")
        deliveryTracker.deleteSpam(spam.keys.toSeq)
      }
      if (fm.nonEmpty) nodeViewHolder ! ModifiersFromRemote(remote, typeId, fm.values.toSeq)
      historyReaderOpt foreach { h =>
        if (!h.isHeadersChainSynced && !deliveryTracker.isExpecting) sendSync(h.syncInfo)
        else if (h.isHeadersChainSynced && !deliveryTracker.isExpectingFromRandom) self ! CheckModifiersToDownload
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) peer.handlerRef ! Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
      deliveryTracker.expect(peer, modifierTypeId, modifierIds)
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) = modifiers.head.modifierTypeId -> modifiers.map(m => m.id -> m.bytes).toMap
        peer.handlerRef ! Message(ModifiersSpec, Right(m), None)
      }
    case a: Any => log.error(s"Strange input (sender: ${sender()}): ${a.getClass}\n" + a)
  }

  case object CheckModifiersToDownload

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(settings.network.syncInterval, settings.network.syncInterval)(self ! CheckModifiersToDownload)
  }

  def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse
      onCheckModifiersToDownload orElse { case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallyFailedModification(mod, throwable) =>
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    case ChangedState(reader) =>
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] => historyReaderOpt = Some(reader)
    case ChangedMempool(reader: EncryMempool) if reader.isInstanceOf[EncryMempool] => mempoolReaderOpt = Some(reader)
    }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
  }

  def onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[EncryBlockHeader] || mod.isInstanceOf[EncryBlockPayload] || mod.isInstanceOf[ADProofs]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) => broadcastModifierInv(mod)
  }

  def onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedExpectingFromRandom()
      historyReaderOpt.foreach { h =>
        val currentQueue: Iterable[ModifierId] = deliveryTracker.expectingFromRandomQueue
        val newIds: Seq[(ModifierTypeId, ModifierId)] = h.modifiersToDownload(settings.network.networkChunkSize - currentQueue.size, currentQueue)
        val oldIds: Seq[(ModifierTypeId, ModifierId)] = deliveryTracker.idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    modifierIds.foreach(id => deliveryTracker.expectFromRandom(modifierTypeId, id))
    val msg: Message[(ModifierTypeId, Seq[ModifierId])] = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    if (settings.node.sendStat)
      context.actorSelection("akka://encry/user/statsSender") ! SendDownloadRequest(modifierTypeId, modifierIds)
    networkController ! SendToNetwork(msg, SendToRandom)
  }
}

object EncryNodeViewSynchronizer {

  object ReceivableMessages {

    case object SendLocalSyncInfo

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    case class CheckDelivery(source: ConnectedPeer,
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: encry.consensus.History.HistoryComparisonResult,
                                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MempoolReader[_ <: Transaction[_]]](mempool: MR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

}