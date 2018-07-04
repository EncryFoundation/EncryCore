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
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.message.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.message._
import encry.settings.NetworkSettings
import encry.utils.EncryLogging
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.history.{EncryHistory, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.{EncryMempool, MempoolReader}
import encry.view.state.{Proposition, StateReader}
import encry.{ModifierId, ModifierTypeId, VersionTag}

class EncryNodeViewSynchronizer(syncInfoSpec: EncrySyncInfoMessageSpec.type) extends Actor with EncryLogging {


  val networkSettings: NetworkSettings = settings.network
  val invSpec: InvSpec = new InvSpec(networkSettings.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  var historyReaderOpt: Option[EncryHistory] = None
  var mempoolReaderOpt: Option[EncryMempool] = None
  val deliveryTracker: ActorRef =
    system.actorOf(Props(classOf[EncryDeliveryManager], networkSettings, self, timeProvider, syncInfoSpec), "deliverTracker")

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    networkController ! SendToNetwork(Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None), Broadcast)

  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => logWarn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  override def receive: Receive = viewHolderEvents orElse {
    case SendLocalSyncInfo => deliveryTracker ! SendLocalSyncInfo
    case OtherNodeSyncingStatus(remote, status, extOpt) => deliveryTracker ! OtherNodeSyncingStatus(remote, status, extOpt)
    case HandshakedPeer(remote) => deliveryTracker ! HandshakedPeer(remote)
    case DisconnectedPeer(remote) => deliveryTracker ! DisconnectedPeer(remote)
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
      deliveryTracker ! DataFromPeer(spec, data: ModifiersData@unchecked, remote)
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      deliveryTracker ! RequestFromLocal(peer, modifierTypeId, modifierIds)
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) = modifiers.head.modifierTypeId -> modifiers.map(m => m.id -> m.bytes).toMap
        peer.handlerRef ! Message(ModifiersSpec, Right(m), None)
      }
    case a: Any => logError(s"Strange input (sender: ${sender()}): ${a.getClass}\n" + a)
  }

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
  }

  def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse { case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallyFailedModification(mod, throwable) =>
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    case ChangedState(reader) =>
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      historyReaderOpt = Some(reader)
      deliveryTracker ! ChangedHistory(reader)
    case ChangedMempool(reader: EncryMempool) if reader.isInstanceOf[EncryMempool] =>
      mempoolReaderOpt = Some(reader)
      deliveryTracker ! ChangedMempool(reader)
    }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      deliveryTracker ! DownloadRequest(modifierTypeId, modifierId)
  }

  def onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[EncryBlockHeader] || mod.isInstanceOf[EncryBlockPayload] || mod.isInstanceOf[ADProofs]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) => broadcastModifierInv(mod)
    case SyntacticallySuccessfulModifier(mod) =>
  }
}

object EncryNodeViewSynchronizer {

  case object CheckModifiersToDownload

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