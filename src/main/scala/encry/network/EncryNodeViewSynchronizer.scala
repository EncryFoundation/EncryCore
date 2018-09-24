package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId, VersionTag}
import encry.EncryApp._
import encry.consensus.History._
import encry.consensus.SyncInfo
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history.{ADProofs, Header, Payload}
import encry.modifiers.mempool.Transaction
import encry.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import encry.network.DeliveryManager.{ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.message.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.message._
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.history.{EncryHistory, EncryHistoryReader, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.{EncryMempool, MempoolReader}
import encry.view.state.StateReader
import encry.utils.Logging
import encry.utils.Utils._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.Proposition

class EncryNodeViewSynchronizer(syncInfoSpec: EncrySyncInfoMessageSpec.type) extends Actor with Logging {

  var historyReaderOpt: Option[EncryHistory] = None
  var mempoolReaderOpt: Option[EncryMempool] = None
  val invSpec: InvSpec = new InvSpec(settings.network.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
  val deliveryManager: ActorRef =
    context.actorOf(Props(classOf[DeliveryManager], syncInfoSpec), "deliveryManager")

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
  }

  override def receive: Receive = {
    case SyntacticallySuccessfulModifier(mod)
      if (mod.isInstanceOf[Header] || mod.isInstanceOf[Payload] || mod.isInstanceOf[ADProofs]) &&
        historyReaderOpt.exists(_.isHeadersChainSynced) => broadcastModifierInv(mod)
    case SyntacticallySuccessfulModifier(mod) =>
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      deliveryManager ! DownloadRequest(modifierTypeId, modifierId)
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallyFailedModification(mod, throwable) =>
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    case ChangedState(reader) =>
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      historyReaderOpt = Some(reader)
      deliveryManager ! ChangedHistory(reader)
    case ChangedMempool(reader: EncryMempool) if reader.isInstanceOf[EncryMempool] =>
      mempoolReaderOpt = Some(reader)
    case SendLocalSyncInfo => deliveryManager ! SendLocalSyncInfo
    case HandshakedPeer(remote) => deliveryManager ! HandshakedPeer(remote)
    case DisconnectedPeer(remote) => deliveryManager ! DisconnectedPeer(remote)
    case DataFromPeer(spec, syncInfo: EncrySyncInfo@unchecked, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>
      logInfo(s"Got sync message from ${remote.socketAddress} with " +
        s"${syncInfo.lastHeaderIds.size} headers. Head's headerId is " +
        s"${Algos.encode(syncInfo.lastHeaderIds.headOption.getOrElse(Array.emptyByteArray))}.")
      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] =
            historyReader.continuationIds(syncInfo, settings.network.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logInfo(s"Comparison with $remote having " +
            s"starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          logInfo(s"Extension ids: ${idsToString(ext)}")
          if (!(extensionOpt.nonEmpty || comparison != Younger))
            logWarn("Extension is empty while comparison is younger")
          deliveryManager ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>
        logInfo(s"Got requestMsg from ${remote.socketAddress}. TypeID:${invData._1}." +
          s" Modifiers: ${invData._2.foldLeft("|")((str, id) => str + "|" + Algos.encode(id))}")
        historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp))).foreach { readers =>
          val objs: Seq[NodeViewModifier] = invData._1 match {
            case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => readers._2.getAll(invData._2)
            case _: ModifierTypeId => invData._2.flatMap(id => readers._1.modifierById(id))
          }
          logDebug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
            s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
          logDebug(s"Peer: ${remote.socketAddress} requested for modifiers of type ${invData._1}.")
          if (invData._1 != Payload.modifierTypeId)
          self ! ResponseFromLocal(remote, invData._1, objs)
        }
    case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == InvSpec.MessageCode =>
      logDebug(s"Got inv message from ${remote.socketAddress}.")
      nodeViewHolder ! CompareViews(remote, invData._1, invData._2)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      logDebug( s"Got modifiers from ${remote.socketAddress} with modTypeID: ${data._1}.")
      deliveryManager ! DataFromPeer(spec, data: ModifiersData@unchecked, remote)
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      deliveryManager ! RequestFromLocal(peer, modifierTypeId, modifierIds)
    case StartMining => deliveryManager ! StartMining
    case DisableMining => deliveryManager ! DisableMining
    case FullBlockChainSynced => deliveryManager ! FullBlockChainSynced
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =
          modifiers.head.modifierTypeId -> modifiers.map(m => m.id -> m.bytes).toMap
        peer.handlerRef ! Message(ModifiersSpec, Right(m), None)
      }
    case StopSync => deliveryManager ! StopSync
    case ContinueSync => deliveryManager ! ContinueSync
    case a: Any => logError(s"Strange input (sender: ${sender()}): ${a.getClass}\n" + a)
  }

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    networkController ! SendToNetwork(Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None), Broadcast)
}

object EncryNodeViewSynchronizer {

  object ReceivableMessages {

    case object CheckModifiersToDownload

    case object SendLocalSyncInfo

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ResponseFromLocal[M <: NodeViewModifier]
    (source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

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

    case class ChangedHistory[HR <: EncryHistoryReader](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MempoolReader[Transaction]](mempool: MR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction](transaction: TX) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
      extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD)
      extends ModificationOutcome

  }

}