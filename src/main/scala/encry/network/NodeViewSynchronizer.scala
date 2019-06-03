package encry.network

import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.AuxiliaryHistoryHolder.AuxHistoryChanged
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.{PeersForSyncInfo, SendToNetwork, UpdatedPeersCollection}
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.Utils._
import encry.view.NodeViewHolder.DownloadRequest
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.history.{EncryHistory, EncryHistoryReader}
import encry.view.mempool.Mempool._
import encry.view.state.StateReader
import org.encryfoundation.common.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scala.concurrent.duration._

class NodeViewSynchronizer(influxRef: Option[ActorRef],
                           nodeViewHolderRef: ActorRef,
                           networkControllerRef: ActorRef,
                           settings: EncryAppSettings,
                           memoryPoolRef: ActorRef,
                           peersKeeperRef: ActorRef) extends Actor with StrictLogging {

  implicit val timeout: Timeout = Timeout(5.seconds)

  var historyReaderOpt: Option[EncryHistory] = None
  var modifiersRequestCache: Map[String, NodeViewModifier] = Map.empty
  var chainSynced: Boolean = false
  val deliveryManager: ActorRef = context.actorOf(
    DeliveryManager.props(influxRef, nodeViewHolderRef, networkControllerRef, settings, memoryPoolRef)
      .withDispatcher("delivery-manager-dispatcher"), "deliveryManager")

  override def preStart(): Unit = {
    val messageIds: Seq[(Byte, String)] = Seq(
      InvNetworkMessage.NetworkMessageTypeID -> "InvNetworkMessage",
      RequestModifiersNetworkMessage.NetworkMessageTypeID -> "RequestModifiersNetworkMessage",
      SyncInfoNetworkMessage.NetworkMessageTypeID -> "SyncInfoNetworkMessage"
    )
    networkControllerRef ! RegisterMessagesHandler(messageIds, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false)
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod) => mod match {
      case block: Block =>
        broadcastModifierInv(block.header)
        modifiersRequestCache = Map(
          Algos.encode(block.id) -> block.header,
          Algos.encode(block.payload.id) -> block.payload
        )
      case tx: Transaction => broadcastModifierInv(tx)
      case _ => //Do nothing
    }
    case DataFromPeer(message, remote) => message match {
      case SyncInfoNetworkMessage(syncInfo) => historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, settings.network.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logger.info(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}.")
          if (!(extensionOpt.nonEmpty || comparison != Younger)) logger.warn("Extension is empty while comparison is younger")
          deliveryManager ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
          peersKeeperRef ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
      case RequestModifiersNetworkMessage(invData) =>
        if (chainSynced) {
          val inRequestCache: Map[String, NodeViewModifier] =
            invData._2.flatMap(id => modifiersRequestCache.get(Algos.encode(id)).map(mod => Algos.encode(mod.id) -> mod)).toMap
          if (invData._1 != Transaction.modifierTypeId)
            logger.debug(s"inRequestCache(${inRequestCache.size}): ${inRequestCache.keys.mkString(",")}")
          sendResponse(remote, invData._1, inRequestCache.values.toSeq)
          val nonInRequestCache: Seq[ModifierId] = invData._2.filterNot(id => inRequestCache.contains(Algos.encode(id)))
          if (nonInRequestCache.nonEmpty) {
            if (invData._1 == Transaction.modifierTypeId) memoryPoolRef ! AskTransactionsFromNVS(remote, nonInRequestCache)
            else historyReaderOpt.foreach { reader =>
              invData._1 match {
                case typeId: ModifierTypeId => nonInRequestCache.foreach(id =>
                  reader.modifierById(id).foreach { mod =>
                    if (typeId != Transaction.modifierTypeId)
                      logger.debug(s"Trigger sendResponse to $remote for modifier $mod of type: $typeId.")
                    sendResponse(remote, invData._1, Seq(mod))
                  }
                )
              }
            }
          }
        }
        else logger.debug(s"Peer $remote requested ${invData._2.length} modifiers ${idsToString(invData)}, but " +
          s"node is not synced, so ignore msg")
      case InvNetworkMessage(invData) =>
        if (invData._1 == Transaction.modifierTypeId) {
          if (chainSynced) memoryPoolRef ! CompareTransactionsWithUnconfirmed(remote, invData._2.toIndexedSeq)
          else logger.debug(s"Get inv with tx: ${invData._2.map(Algos.encode).mkString(",")}") // do nothing
        }
        else if (invData._1 != Payload.modifierTypeId) {
          logger.debug(s"Got inv message on NodeViewSynchronizer from ${remote.socketAddress} with modifiers of type:" +
            s" ${invData._1}. Size of inv is: ${invData._2.size}. Sending CompareViews to NVH. " +
            s"\nModifiers in inv message are: ${invData._2.map(Algos.encode).mkString(",")}")
          nodeViewHolderRef ! CompareViews(remote, invData._1, invData._2)
        } //todo: Ban node that send payload id?
      case _ => logger.debug(s"NodeViewSyncronyzer got invalid type of DataFromPeer message!")
    }
    case msg@RequestFromLocal(_, _, _) => deliveryManager ! msg
    case msg@PeersForSyncInfo(_) => deliveryManager ! msg
    case msg@DownloadRequest(_, _, _) => deliveryManager ! msg
    case msg@UpdatedPeersCollection(_) => deliveryManager ! msg
    case msg@PeersForSyncInfo(_) => deliveryManager ! msg
    case msg@RequestForTransactions(_, _, _) => deliveryManager ! msg
    case msg@StartMining => deliveryManager ! msg
    case msg@DisableMining => deliveryManager ! msg
    case msg@AccumulatedPeersStatistic(_) => peersKeeperRef ! msg
    case AuxHistoryChanged(history) => historyReaderOpt = Some(history)
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      deliveryManager ! UpdatedHistory(reader)
    case TxsForNVSH(remote, txs) => sendResponse(remote, Transaction.modifierTypeId, txs)
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SemanticallyFailedModification(_, _) =>
    case ChangedState(_) =>
    case SyntacticallyFailedModification(_, _) =>
    case FullBlockChainIsSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainIsSynced
    case a: Any => logger.error(s"Strange input(sender: ${sender()}): ${a.getClass}\n" + a)
  }

  def sendResponse(peer: ConnectedPeer, typeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit =
    if (modifiers.nonEmpty) {
      if (typeId != Transaction.modifierTypeId)
        logger.debug(s"Sent modifiers to $peer size is: ${modifiers.length}|${modifiers.map(mod => Algos.encode(mod.id)).mkString(",")}")
      typeId match {
        case Header.modifierTypeId =>
          val modsB: Seq[(ModifierId, Array[Byte])] =
            modifiers.map { case h: Header => h.id -> HeaderProtoSerializer.toProto(h).toByteArray }
          logger.debug(s"Sent to peer handler for $peer ModfiersNetworkMessage for HEADERS with ${modsB.size} headers." +
            s" \n Headers are: ${modsB.map(x => Algos.encode(x._1)).mkString(",")}.")
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modsB.toMap)
        case Payload.modifierTypeId =>
          val modsB: Seq[(ModifierId, Array[Byte])] =
            modifiers.map { case h: Payload => h.id -> PayloadProtoSerializer.toProto(h).toByteArray }
          logger.debug(s"Sent to peer handler for $peer ModfiersNetworkMessage for PAYLOADS with ${modsB.size} payloads." +
            s" \n Payloads are: ${modsB.map(x => Algos.encode(x._1)).mkString(",")}.")
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modsB.toMap)
        case Transaction.modifierTypeId =>
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modifiers.map {
            case h: Transaction => h.id -> TransactionProtoSerializer.toProto(h).toByteArray
          }.toMap)
      }
    }

  def broadcastModifierInv(m: NodeViewModifier): Unit =
    if (chainSynced) networkControllerRef ! SendToNetwork(InvNetworkMessage(m.modifierTypeId -> Seq(m.id)), Broadcast)
}

object NodeViewSynchronizer {

  object ReceivableMessages {

    final case object SendLocalSyncInfo

    final case class OtherNodeSyncingStatus(remote: ConnectedPeer,
                                            status: encry.consensus.History.HistoryComparisonResult,
                                            extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    case class ResponseFromLocal[M <: NodeViewModifier]
    (source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    final case class RequestFromLocal(source: ConnectedPeer,
                                      modifierTypeId: ModifierTypeId,
                                      modifierIds: Seq[ModifierId])

    trait PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: EncryHistoryReader](reader: HR) extends NodeViewChange

    case class UpdatedHistory(history: EncryHistory)

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SuccessfulTransaction(transaction: Transaction) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

  def props(influxRef: Option[ActorRef],
            nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: EncryAppSettings,
            memoryPoolRef: ActorRef,
            peersKeeperRef: ActorRef): Props =
    Props(new NodeViewSynchronizer(influxRef, nodeViewHolderRef, networkControllerRef, settings, memoryPoolRef, peersKeeperRef))

  class NodeViewSynchronizerPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case RequestFromLocal(_, _, _) => 0

        case DataFromPeer(msg, _) => msg match {
          case SyncInfoNetworkMessage(_) => 1
          case InvNetworkMessage(data) if data._1 != Transaction.modifierTypeId => 1
          case RequestModifiersNetworkMessage(data) if data._1 != Transaction.modifierTypeId => 2
          case _ => 4
        }

        case SemanticallySuccessfulModifier(mod) => mod match {
          case tx: Transaction => 4
          case _ => 1
        }

        case SuccessfulTransaction(_) => 4

        case PoisonPill => 5

        case otherwise => 3
      })

}