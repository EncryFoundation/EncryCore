package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import encry.EncryApp._
import encry.consensus.History._
import encry.consensus.{HistoryReader, SyncInfo}
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.Transaction
import encry.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import encry.network.EncryDeliveryTracker.{CheckModifiersToDownload, FilteredModifiers, NeedToDownloadFromRandom}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.message.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.message._
import encry.settings.NetworkSettings
import encry.utils.ScorexLogging
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges, ModifiersFromRemote}
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.{EncryMempool, MempoolReader}
import encry.view.state.{Proposition, StateReader}
import encry.{ModifierId, ModifierTypeId, VersionTag}

class EncryNodeViewSynchronizer(syncInfoSpec: EncrySyncInfoMessageSpec.type) extends Actor with ScorexLogging {


  val networkSettings: NetworkSettings = settings.network
  val deliveryTracker: ActorRef =
    context.actorOf(Props(classOf[EncryDeliveryTracker], networkSettings, timeProvider, self))
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
      networkController ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(Seq(peers.last)))
  }

  // Send history extension to the (less developed) peer 'remote' which does not have it.
  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => log.warn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  override def receive: Receive = viewHolderEvents orElse {
    case SendLocalSyncInfo =>
      if (statusTracker.elapsedTimeSinceLastSync() < networkSettings.syncInterval.toMillis)
        log.info("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => log.warn("Peer status is still unknown")
        case Nonsense => log.warn("Got nonsense")
        case Younger => sendExtension(remote, status, extOpt)
        case _ =>
      }
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
    case DataFromPeer(spec, syncInfo: EncrySyncInfo@unchecked, remote) if spec.messageCode == syncInfoSpec.messageCode =>
      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, networkSettings.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          log.info(s"Comparison with $remote having starting points ${encry.idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          log.info(s"Extension ids: ${encry.idsToString(ext)}")
          if (!(extensionOpt.nonEmpty || comparison != Younger)) log.warn("Extension is empty while comparison is younger")
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
      println("Get inv spec")
      nodeViewHolder ! CompareViews(remote, invData._1, invData._2)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      deliveryTracker ! DataFromPeer(spec, data: ModifiersData@unchecked, remote)
    case FilteredModifiers(typeId: ModifierTypeId, modifiers: Seq[Array[Byte]]) =>
      nodeViewHolder ! ModifiersFromRemote(typeId, modifiers)
      //println(s"Current historyReaderOpt: ${historyReaderOpt.map(_.bestHeaderOpt.map(header => Algos.encode(header.id) + s"Height: ${header.height}"))}")
    case SendSync => historyReaderOpt.foreach(history => sendSync(history.syncInfo))

    case SendHistoryToDelTreacker => historyReaderOpt.foreach(history => deliveryTracker ! CheckModifiersToDownload(history))

    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      println(s"In RFL: ${modifierIds.size}")
      deliveryTracker ! RequestFromLocal(peer, modifierTypeId, modifierIds)
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) = modifiers.head.modifierTypeId -> modifiers.map(m => m.id -> m.bytes).toMap
        peer.handlerRef ! Message(ModifiersSpec, Right(m), None)
      }
    case a: Any => log.error(s"Strange input (sender: ${sender()}): ${a.getClass}\n" + a)
  }

  case object SendHistoryToDelTreacker

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
    statusTracker.scheduleSendSyncInfo()
    context.system.scheduler.schedule(settings.network.syncInterval, settings.network.syncInterval)(self ! SendSync)
  }

  def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse { case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallyFailedModification(mod, throwable) =>
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    case ChangedState(reader) =>
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] => historyReaderOpt = Some(reader)
    case ChangedMempool(reader: EncryMempool) if reader.isInstanceOf[EncryMempool] => mempoolReaderOpt = Some(reader)
    }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      deliveryTracker ! NeedToDownloadFromRandom(modifierTypeId, Seq(modifierId))
  }

  def onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[EncryBlockHeader] || mod.isInstanceOf[EncryBlockPayload]
      || mod.isInstanceOf[ADProofs]) && historyReaderOpt.exists(_.isHeadersChainSynced) => broadcastModifierInv(mod)
    case SyntacticallySuccessfulModifier(mod) =>
  }
}

object EncryNodeViewSynchronizer {

  object ReceivableMessages {

    case object SendSync

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