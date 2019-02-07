package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId, VersionTag}
import encry.EncryApp._
import encry.consensus.History._
import encry.consensus.SyncInfo
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history.{ADProofs, Block, Header, Payload}
import encry.modifiers.mempool.Transaction
import encry.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import encry.network.AuxiliaryHistoryHolder.AuxHistoryChanged
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.DeliveryManager.{ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.message.BasicMsgDataTypes.{InvData, ModifiersData}
import encry.network.message._
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.history.{EncryHistory, EncryHistoryReader, EncrySyncInfo, EncrySyncInfoMessageSpec}
import encry.view.mempool.{Mempool, MempoolReader}
import encry.view.state.StateReader
import encry.utils.Utils._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.Proposition

class NodeViewSynchronizer extends Actor with StrictLogging {

  var historyReaderOpt: Option[EncryHistory] = None
  var mempoolReaderOpt: Option[Mempool] = None
  val invSpec: InvSpec = new InvSpec(settings.network.maxInvObjects)
  var chainSynced: Boolean = false
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
  val deliveryManager: ActorRef =
    context.actorOf(Props(classOf[DeliveryManager]), "deliveryManager")

  override def preStart(): Unit = {
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, EncrySyncInfoMessageSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
  }

  override def receive: Receive = {
    case SyntacticallySuccessfulModifier(mod)
      if (mod.isInstanceOf[Header] || mod.isInstanceOf[Payload] || mod.isInstanceOf[ADProofs]) &&
        historyReaderOpt.exists(_.isHeadersChainSynced) =>
    case SyntacticallySuccessfulModifier(_) =>
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifiersId: Seq[ModifierId]) =>
      deliveryManager ! DownloadRequest(modifierTypeId, modifiersId)
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallyFailedModification(_, _) =>
    case SemanticallySuccessfulModifier(mod) =>
      mod match {
        case block: Block => broadcastModifierInv(block.header)
        case tx: Transaction => broadcastModifierInv(tx)
        case mod => logger.info("Broadcast only block or tx id")
      }
    case SemanticallyFailedModification(_, _) =>
    case ChangedState(_) =>
    case AuxHistoryChanged(history) => historyReaderOpt = Some(history)
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      deliveryManager ! ChangedHistory(reader)
    case ChangedMempool(reader: Mempool) if reader.isInstanceOf[Mempool] =>
      mempoolReaderOpt = Some(reader)
    case SendLocalSyncInfo => deliveryManager ! SendLocalSyncInfo
    case HandshakedPeer(remote) => deliveryManager ! HandshakedPeer(remote)
    case DisconnectedPeer(remote) => deliveryManager ! DisconnectedPeer(remote)
    case DataFromPeer(spec, syncInfo: EncrySyncInfo@unchecked, remote)
      if spec.messageCode == EncrySyncInfoMessageSpec.messageCode =>
      logger.info(s"Got sync message from ${remote.socketAddress} with " +
        s"${syncInfo.lastHeaderIds.size} headers. Head's headerId is: " +
        s"${Algos.encode(syncInfo.lastHeaderIds.headOption.getOrElse(Array.emptyByteArray))}.")
      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, settings.network.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logger.info(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          if (!(extensionOpt.nonEmpty || comparison != Younger)) logger.warn("Extension is empty while comparison is younger")
          deliveryManager ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
    case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == RequestModifierSpec.MessageCode =>
      logger.info(s"Get request from remote peer. chainSynced = $chainSynced")
      if (chainSynced) {
        historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp))).foreach { readers =>
          val objs: Seq[NodeViewModifier] = invData._1 match {
            case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => readers._2.getAll(invData._2)
            case _: ModifierTypeId => invData._2.flatMap(id => readers._1.modifierById(id))
          }
          logger.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
            s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
          self ! ResponseFromLocal(remote, invData._1, objs)
        }
      }
      else logger.info(s"Peer $remote requested ${invData._2.length} modifiers ${idsToString(invData)}, but " +
        s"node is not synced, so ignore msg")
    case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == InvSpec.MessageCode =>
      logger.debug(s"Got inv message from ${remote.socketAddress}")
      nodeViewHolder ! CompareViews(remote, invData._1, invData._2)
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      deliveryManager ! DataFromPeer(spec, data: ModifiersData@unchecked, remote)
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      deliveryManager ! RequestFromLocal(peer, modifierTypeId, modifierIds)
    case StartMining => deliveryManager ! StartMining
    case DisableMining => deliveryManager ! DisableMining
    case FullBlockChainSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainSynced
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) =
          modifiers.head.modifierTypeId -> modifiers.map(m => m.id -> m.bytes).toMap
        peer.handlerRef ! Message(ModifiersSpec, Right(m), None)
      }
    case StopSync => deliveryManager ! StopSync
    case ContinueSync => deliveryManager ! ContinueSync
    case a: Any => logger.error(s"Strange input(sender: ${sender()}): ${a.getClass}\n" + a)
  }

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    if (chainSynced)
      networkController ! SendToNetwork(Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None), Broadcast)

}

object NodeViewSynchronizer {

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

    case class ChangedMempool[MR <: MempoolReader](mempool: MR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction](transaction: TX) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome
  }

}