package encry.network

import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.consensus.SyncInfo
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history._
import encry.modifiers.mempool.{Transaction, TransactionProtoSerializer}
import encry.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import encry.network.AuxiliaryHistoryHolder.AuxHistoryChanged
import encry.network.BasicMessagesRepo._
import encry.network.DeliveryManager.{FullBlockChainIsSynced, ModifiersFromNVH}
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId, VersionTag}
import encry.utils.Utils._
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.history.{EncryHistory, EncryHistoryReader}
import encry.view.mempool.Mempool._
import encry.view.state.StateReader
import org.encryfoundation.common.Algos
import scala.concurrent.duration._

class NodeViewSynchronizer(influxRef: Option[ActorRef],
                           nodeViewHolderRef: ActorRef,
                           networkControllerRef: ActorRef,
                           system: ActorSystem,
                           settings: EncryAppSettings,
                           memoryPoolRef: ActorRef) extends Actor with StrictLogging {

  implicit val timeout: Timeout = Timeout(5.seconds)

  var historyReaderOpt: Option[EncryHistory] = None
  var modifiersRequestCache: Map[String, NodeViewModifier] = Map.empty
  var chainSynced: Boolean = false
  val deliveryManager: ActorRef = context.actorOf(
    DeliveryManager.props(influxRef, nodeViewHolderRef, networkControllerRef, settings, memoryPoolRef)
      .withDispatcher("delivery-manager-dispatcher"), "deliveryManager")

  override def preStart(): Unit = {
    val messageIds: Seq[Byte] = Seq(
      InvNetworkMessage.NetworkMessageTypeID,
      RequestModifiersNetworkMessage.NetworkMessageTypeID,
      SyncInfoNetworkMessage.NetworkMessageTypeID)
    networkControllerRef ! RegisterMessagesHandler(messageIds, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false)
  }

  override def receive: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId, previousModifier: Option[ModifierId]) =>
      deliveryManager ! DownloadRequest(modifierTypeId, modifierId, previousModifier)
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SemanticallyFailedModification(_, _) =>
    case ChangedState(_) =>
    case SyntacticallyFailedModification(_, _) =>
    case ModifiersFromNVH(fm) => deliveryManager ! ModifiersFromNVH(fm)
    case SemanticallySuccessfulModifier(mod) =>
      mod match {
        case block: Block =>
          broadcastModifierInv(block.header)
          modifiersRequestCache = Map(
            Algos.encode(block.id) -> block.header,
            Algos.encode(block.payload.id) -> block.payload
          )
        case tx: Transaction => broadcastModifierInv(tx)
        case _ => //Do nothing
      }
    case AuxHistoryChanged(history) => historyReaderOpt = Some(history)
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      deliveryManager ! UpdatedHistory(reader)
    case HandshakedPeer(remote) => deliveryManager ! HandshakedPeer(remote)
    case DisconnectedPeer(remote) => deliveryManager ! DisconnectedPeer(remote)
    case DataFromPeer(message, remote) => message match {
      case SyncInfoNetworkMessage(syncInfo) =>
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
      case RequestModifiersNetworkMessage(invData) =>
        logger.info(s"Get request modifiers from $remote. chainSynced = $chainSynced")
        if (chainSynced) {
          val inRequestCache: Map[String, NodeViewModifier] =
            invData._2.flatMap(id => modifiersRequestCache.get(Algos.encode(id)).map(mod => Algos.encode(mod.id) -> mod)).toMap
          logger.debug(s"inRequestCache(${inRequestCache.size}): ${inRequestCache.keys.mkString(",")}")
          sendResponse(remote, invData._1, inRequestCache.values.toSeq)
          val nonInRequestCache: Seq[ModifierId] = invData._2.filterNot(id => inRequestCache.contains(Algos.encode(id)))
          if (nonInRequestCache.nonEmpty) {
            if (invData._1 == Transaction.ModifierTypeId) memoryPoolRef ! AskTransactionsFromNVS(remote ,nonInRequestCache)
            else historyReaderOpt.foreach { reader =>
              invData._1 match {
                case _: ModifierTypeId => nonInRequestCache.foreach(id =>
                  reader.modifierById(id).foreach(mod => sendResponse(remote, invData._1, Seq(mod)))
                )
              }
            }
          }
        }
        else logger.info(s"Peer $remote requested ${invData._2.length} modifiers ${idsToString(invData)}, but " +
          s"node is not synced, so ignore msg")
      case InvNetworkMessage(invData) =>
        logger.info(s"Got inv message from ${remote.socketAddress} with modifiers: ${invData._2.map(Algos.encode).mkString(",")} ")
        if (invData._1 == Transaction.ModifierTypeId && chainSynced)
          memoryPoolRef ! CompareTransactionsWithUnconfirmed(remote, invData._2.toIndexedSeq)
        else if (invData._1 != Payload.modifierTypeId) nodeViewHolderRef ! CompareViews(remote, invData._1, invData._2) //todo: Ban node that send payload id?
      case _ => logger.info(s"NodeViewSyncronyzer got invalid type of DataFromPeer message!")
    }
    case TxsForNVSH(remote, txs) => sendResponse(remote, Transaction.ModifierTypeId, txs)
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      deliveryManager ! RequestFromLocal(peer, modifierTypeId, modifierIds)
    case StartMining => deliveryManager ! StartMining
    case DisableMining => deliveryManager ! DisableMining
    case FullBlockChainIsSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainIsSynced
    case r@RequestForTransactions(_, _, _) => deliveryManager ! r
    case a: Any => logger.error(s"Strange input(sender: ${sender()}): ${a.getClass}\n" + a)
  }

  def sendResponse(peer: ConnectedPeer, typeId: ModifierTypeId, modifiers: Seq[NodeViewModifier]): Unit =
    if (modifiers.nonEmpty) {
      logger.info(s"Sent modifiers size is: ${modifiers.length}|${modifiers.map(mod => Algos.encode(mod.id)).mkString(",")}")
      typeId match {
        case Header.modifierTypeId =>
          val modsB: Seq[(ModifierId, Array[Byte])] =
            modifiers.map { case h: Header => h.id -> HeaderProtoSerializer.toProto(h).toByteArray }
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modsB.toMap)
        case Payload.modifierTypeId =>
          val modsB: Seq[(ModifierId, Array[Byte])] =
            modifiers.map { case h: Payload => h.id -> PayloadProtoSerializer.toProto(h).toByteArray }
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modsB.toMap)
        case Transaction.ModifierTypeId =>
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modifiers.map {
            case h: Transaction => h.id -> TransactionProtoSerializer.toProto(h).toByteArray
          }.toMap)
      }
    }

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    if (chainSynced) networkControllerRef ! SendToNetwork(InvNetworkMessage(m.modifierTypeId -> Seq(m.id)), Broadcast)
}

object NodeViewSynchronizer {

  object ReceivableMessages {

    case object SendLocalSyncInfo

    case object CheckModifiersToDownload

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: encry.consensus.History.HistoryComparisonResult,
                                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    case class ResponseFromLocal[M <: NodeViewModifier]
    (source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    //    case class CheckDelivery(source: ConnectedPeer,
    //                             modifierTypeId: ModifierTypeId,
    //                             modifierId: ModifierId)

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

  class NodeViewSynchronizerPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case RequestFromLocal(_, _, _) => 0

        case DataFromPeer(msg, _) => msg match {
          case SyncInfoNetworkMessage(_) => 1
          case InvNetworkMessage(data) if data._1 != Transaction.ModifierTypeId => 1
          case RequestModifiersNetworkMessage(data) if data._1 != Transaction.ModifierTypeId => 1
          case _ => 3
        }

        case SemanticallySuccessfulModifier(mod) => mod match {
          case tx: Transaction => 3
          case _ => 1
        }

        case SuccessfulTransaction(_) => 3

        case PoisonPill => 4

        case otherwise => 2
      })
}