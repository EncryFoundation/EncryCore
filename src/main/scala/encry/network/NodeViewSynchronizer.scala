package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.{PeersForSyncInfo, SendToNetwork}
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
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
import org.encryfoundation.common.network.BasicMessagesRepo.{InvNetworkMessage, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
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
      InvNetworkMessage.NetworkMessageTypeID              -> "InvNetworkMessage",
      RequestModifiersNetworkMessage.NetworkMessageTypeID -> "RequestModifiersNetworkMessage",
      SyncInfoNetworkMessage.NetworkMessageTypeID         -> "SyncInfoNetworkMessage"
    )
    networkControllerRef ! RegisterMessagesHandler(messageIds, self)
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false)
  }

  override def receive: Receive = awaitingHistoryCycle

  def awaitingHistoryCycle: Receive = {
    case ChangedHistory(reader: EncryHistory) =>
      logger.info(s"get history: $reader from $sender")
      deliveryManager ! UpdatedHistory(reader)
      context.become(workingCycle(reader))
  }

  def workingCycle(encryHistory: EncryHistory): Receive = {
    case msg@CheckModifiersToDownload(_) => deliveryManager ! msg
    case msg@CheckModifiersToDownloadSuccess => peersKeeperRef ! msg
    case msg@AccumulatedPeersStatistic(_) => peersKeeperRef ! msg
    case msg@PeersForSyncInfo(_) => deliveryManager ! msg
    case msg@DownloadRequest(_, _, _, _) => deliveryManager ! msg
    //      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"NVSH got download request from $sender for modfiier of type:" +
    //        s" $modifierTypeId with id: ${Algos.encode(modifierId)}. PrevMod is: ${previousModifier.map(Algos.encode)}." +
    //        s"Sending this message to DM.")
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId, previousModifier: Option[ModifierId]) =>
      if(modifierTypeId != Transaction.modifierTypeId) logger.debug(s"NVSH got download request from $sender for modfiier of type:" +
        s" $modifierTypeId with id: ${Algos.encode(modifierId)}. PrevMod is: ${previousModifier.map(Algos.encode)}." +
        s"Sending this message to DM.")
      deliveryManager ! DownloadRequest(modifierTypeId, modifierId, previousModifier)
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case msg@PeersForSyncInfo(_) => deliveryManager ! msg
    case SemanticallyFailedModification(_, _) =>
    case SyntacticallyFailedModification(_, _) =>
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
    //case AuxHistoryChanged(history) => historyReaderOpt = Some(history)
    case ChangedHistory(reader: EncryHistory) =>
      logger.info(s"get history: $reader from $sender")
      context.become(workingCycle(reader))
      deliveryManager ! UpdatedHistory(reader)
    case HandshakedPeer(remote) => deliveryManager ! HandshakedPeer(remote)
    case DisconnectedPeer(remote) => deliveryManager ! DisconnectedPeer(remote)
    case DataFromPeer(message, remote) => message match {
      case SyncInfoNetworkMessage(syncInfo) =>
        logger.info(s"Got sync message from ${remote.socketAddress} with " +
          s"${syncInfo.lastHeaderIds.size} headers. Head's headerId is: " +
          s"${Algos.encode(syncInfo.lastHeaderIds.headOption.getOrElse(Array.emptyByteArray))}. History: ${encryHistory}")
        val testHistory = Option(encryHistory)
        testHistory match {
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
        if (invData._1 != Transaction.modifierTypeId)
          logger.debug(s"Got request modifiers from $remote for modifiers of type: ${invData._1} on NVSH. chainSynced = $chainSynced." +
            s" Number of requesting modifiers is: ${invData._2.size}.")
        if (chainSynced) {
          val inRequestCache: Map[String, NodeViewModifier] =
            invData._2.flatMap(id => modifiersRequestCache.get(Algos.encode(id)).map(mod => Algos.encode(mod.id) -> mod)).toMap
          if (invData._1 != Transaction.modifierTypeId)
            logger.debug(s"inRequestCache(${inRequestCache.size}): ${inRequestCache.keys.mkString(",")}")
          sendResponse(remote, invData._1, inRequestCache.values.collect {
            case header: Header => header.id -> HeaderProtoSerializer.toProto(header).toByteArray
            case payload: Payload => payload.id -> PayloadProtoSerializer.toProto(payload).toByteArray
            case adProof: ADProofs => adProof.id -> ADProofsProtoSerializer.toProto(adProof).toByteArray
          }.toSeq)
          val nonInRequestCache: Seq[ModifierId] = invData._2.filterNot(id => inRequestCache.contains(Algos.encode(id)))
          if (nonInRequestCache.nonEmpty) {
            if (invData._1 == Transaction.modifierTypeId) memoryPoolRef ! AskTransactionsFromNVS(remote, nonInRequestCache)
            else Option(encryHistory).foreach { reader =>
              invData._1 match {
                case typeId: ModifierTypeId => nonInRequestCache.foreach(id =>
                  reader.modifierBytesById(id).foreach { mod =>
                    if (typeId != Transaction.modifierTypeId)
                      logger.debug(s"Trigger sendResponse to $remote for modifier $mod of type: $typeId.")
                    sendResponse(remote, invData._1, Seq(id -> mod))
                  }
                )
              }
            }
          }
        }
        else logger.debug(s"Peer $remote requested ${invData._2.length} modifiers ${idsToString(invData)}, but " +
          s"node is not synced, so ignore msg")
      case InvNetworkMessage(invData) =>
        //logger.info(s"Got inv message from ${remote.socketAddress} with modifiers: ${invData._2.map(Algos.encode).mkString(",")} ")
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
    case TxsForNVSH(remote, txs) => sendResponse(
      remote, Transaction.modifierTypeId, txs.map(tx => tx.id -> TransactionProtoSerializer.toProto(tx).toByteArray)
    )
    case msg@RequestFromLocal(peer, modifierTypeId, modifierIds, peers) =>
      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got RequestFromLocal on NVSH from $sender with " +
        s"ids of type: $modifierTypeId. Number of ids is: ${modifierIds.size}. Sending request from local to DeliveryManager." +
        s" \nIds are: ${modifierIds.map(Algos.encode).mkString(",")}")
      deliveryManager ! msg
    case StartMining => deliveryManager ! StartMining
    case DisableMining => deliveryManager ! DisableMining
    case FullBlockChainIsSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainIsSynced
    case r@RequestForTransactions(_, _, _) => deliveryManager ! r
    case a: Any => logger.error(s"Strange input(sender: ${sender()}): ${a.getClass}\n" + a)
  }

  def sendResponse(peer: ConnectedPeer, typeId: ModifierTypeId, modifiersBytes: Seq[(ModifierId, Array[Byte])]): Unit =
    if (modifiersBytes.nonEmpty) {
      if (typeId != Transaction.modifierTypeId)
        logger.debug(s"Sent modifiers to $peer size is: ${modifiersBytes.length}")
      typeId match {
        case Header.modifierTypeId =>
          logger.debug(s"Sent to peer handler for $peer ModfiersNetworkMessage for HEADERS with ${modifiersBytes.size} headers." +
            s" \n Headers are: ${modifiersBytes.map(x => Algos.encode(x._1)).mkString(",")}.")
          peer.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersBytes.toMap)
        case Payload.modifierTypeId =>
          logger.debug(s"Sent to peer handler for $peer ModfiersNetworkMessage for PAYLOADS with ${modifiersBytes.size} payloads." +
            s" \n Payloads are: ${modifiersBytes.map(x => Algos.encode(x._1)).mkString(",")}.")
          peer.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersBytes.toMap)
        case Transaction.modifierTypeId =>
          peer.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersBytes.toMap)
      }
    }

  def broadcastModifierInv(m: NodeViewModifier): Unit =
    if (chainSynced) networkControllerRef ! SendToNetwork(InvNetworkMessage(m.modifierTypeId -> Seq(m.id)), Broadcast)
}

object NodeViewSynchronizer {

  object ReceivableMessages {

    final case object SendLocalSyncInfo

    final case class CheckModifiersToDownload(peers: IndexedSeq[(ConnectedPeer, PeersPriorityStatus)]) extends AnyVal
    final case object CheckModifiersToDownloadSuccess

    case class OtherNodeSyncingStatus(remote: ConnectedPeer,
                                      status: encry.consensus.History.HistoryComparisonResult,
                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    case class ResponseFromLocal[M <: NodeViewModifier]
    (source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    final case class GetPeersForRequestFromLocal(source: ConnectedPeer,
                                                 modifierTypeId: ModifierTypeId,
                                                 modifierIds: Seq[ModifierId])
    case class RequestFromLocal(source: ConnectedPeer,
                                modifierTypeId: ModifierTypeId,
                                modifierIds: Seq[ModifierId],
                                peers: Map[ConnectedPeer, HistoryComparisonResult])

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
        case RequestFromLocal(_, _, _, _) => 0

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