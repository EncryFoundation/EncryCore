package encry.network

import java.net.InetSocketAddress

import akka.actor.Actor
import encry.EncryApp._
import encry.consensus.{HistoryReader, SyncInfo}
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes._
import encry.network.message.{InvSpec, RequestModifierSpec, _}
import encry.settings.NetworkSettings
import encry.utils.ScorexLogging
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.StateReader
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.{PersistentNodeViewModifier, _}
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * A component which synchronizes local node view with p2p network.
  *
  * @param syncInfoSpec SyncInfo specification
  * @tparam P   proposition
  * @tparam TX  transaction
  * @tparam SIS SyncInfoMessage specification
  */
class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P], SI <: SyncInfo,
SIS <: SyncInfoMessageSpec[SI], PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, SI], MR <: MempoolReader[TX]]
(syncInfoSpec: SIS) extends Actor with ScorexLogging {

  import NodeViewSynchronizer.ReceivableMessages._
  import encry.consensus.History._
  import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
  import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges, ModifiersFromRemote}

  val networkSettings: NetworkSettings = settings.network

  val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  val invSpec: InvSpec = new InvSpec(networkSettings.maxInvObjects)
  val requestModifierSpec: RequestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  val deliveryTracker: DeliveryTracker = new DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self)
  val statusTracker: SyncTracker = SyncTracker(self, context, networkSettings, timeProvider)

  var historyReaderOpt: Option[HR] = None
  var mempoolReaderOpt: Option[MR] = None

  def readersOpt: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    networkController ! SendToNetwork(Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None), Broadcast)

  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  def viewHolderEvents: Receive = {
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallyFailedModification(mod, throwable) =>
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    case ChangedHistory(reader: HR@unchecked) if reader.isInstanceOf[HR] => historyReaderOpt = Some(reader)
    case ChangedMempool(reader: MR@unchecked) if reader.isInstanceOf[MR] => mempoolReaderOpt = Some(reader)
  }

  def sendSync(syncInfo: SI): Unit = {
    val peers = statusTracker.peersToSyncWith()
    if (peers.nonEmpty)
      networkController ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peers))
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

  override def receive: Receive =
    viewHolderEvents orElse {
      case DataFromPeer(spec, syncInfo: SI@unchecked, remote)
        if spec.messageCode == syncInfoSpec.messageCode =>
        historyReaderOpt match {
          case Some(historyReader) =>
            val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, networkSettings.networkChunkSize)
            val ext: ModifierIds = extensionOpt.getOrElse(Seq())
            val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
            log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
              s"Comparison result is $comparison. Sending extension of length ${ext.length}")
            log.trace(s"Extension ids: ${idsToString(ext)}")
            if (!(extensionOpt.nonEmpty || comparison != Younger)) log.warn("Extension is empty while comparison is younger")
            self ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
          case _ =>
        }
      case SendLocalSyncInfo =>
        if (statusTracker.elapsedTimeSinceLastSync() < (networkSettings.syncInterval.toMillis / 2))
          log.debug("Trying to send sync info too often")
        else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
      case OtherNodeSyncingStatus(remote, status, extOpt) =>
        statusTracker.updateStatus(remote, status)
        status match {
          case Unknown => log.warn("Peer status is still unknown")
          case Nonsense => log.warn("Got nonsense") //todo: fix, see https://github.com/ScorexFoundation/Scorex/issues/158
          case Younger => sendExtension(remote, status, extOpt)
          case _ => // does nothing for `Equal` and `Older`
        }
      case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
      case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
      case CheckDelivery(peer, modifierTypeId, modifierId) =>
        if (deliveryTracker.peerWhoDelivered(modifierId).contains(peer)) deliveryTracker.delete(modifierId)
        else {
          log.info(s"Peer $peer has not delivered asked modifier ${Base58.encode(modifierId)} on time")
          deliveryTracker.reexpect(peer, modifierTypeId, modifierId)
        }
      case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == RequestModifierSpec.MessageCode =>
        readersOpt.foreach { readers =>
          val objs: Seq[NodeViewModifier] = invData._1 match {
            case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => readers._2.getAll(invData._2)
            case _: ModifierTypeId => invData._2.flatMap(id => readers._1.modifierById(id))
          }
          log.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
            s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
          self ! ResponseFromLocal(remote, invData._1, objs)
        }
      case DataFromPeer(spec, invData: InvData@unchecked, remote) if spec.messageCode == InvSpec.MessageCode =>
        nodeViewHolder ! CompareViews(remote, invData._1, invData._2)
      case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
        if (modifierIds.nonEmpty) {
          val msg: Message[(ModifierTypeId, Seq[ModifierId])] =
            Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
          peer.handlerRef ! msg
        }
        deliveryTracker.expect(peer, modifierTypeId, modifierIds)
      case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
        if (modifiers.nonEmpty) {
          @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
          val modType: ModifierTypeId = modifiers.head.modifierTypeId
          val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) = modType -> modifiers.map(m => m.id -> m.bytes).toMap
          val msg: Message[(ModifierTypeId, Map[ModifierId, Array[Byte]])] = Message(ModifiersSpec, Right(m), None)
          peer.handlerRef ! msg
        }
      case a: Any => log.error("Strange input: " + a)
    }
}

object NodeViewSynchronizer {

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

    //TODO: return mempool reader
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