package encry.network

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.cli.commands.AddPeer.PeerFromCli
import encry.cli.commands.RemoveFromBlackList.RemovePeerFromBlackList
import encry.consensus.History._
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.BlackList.SentInvForPayload
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DownloadedModifiersValidator.InvalidModifiers
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.{BanPeer, ConnectionStopped, PeersForSyncInfo, RequestPeersForFirstSyncInfo, SendToNetwork, UpdatedPeersCollection}
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.Utils._
import encry.view.NodeViewHolder.DownloadRequest
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges, ModifiersFromRemote}
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
                           settings: EncryAppSettings,
                           memoryPoolRef: ActorRef,
                           dataHolder: ActorRef) extends Actor with StrictLogging {

  val peersKeeper: ActorRef = context.system.actorOf(PeersKeeper.props(settings, self, dataHolder)
    .withDispatcher("peers-keeper-dispatcher"))

  val networkController: ActorRef = context.system.actorOf(NetworkController.props(settings, peersKeeper, self)
    .withDispatcher("network-dispatcher"))

  networkController ! RegisterMessagesHandler(Seq(
    InvNetworkMessage.NetworkMessageTypeID              -> "InvNetworkMessage",
    RequestModifiersNetworkMessage.NetworkMessageTypeID -> "RequestModifiersNetworkMessage",
    SyncInfoNetworkMessage.NetworkMessageTypeID         -> "SyncInfoNetworkMessage"
  ), self)

  implicit val timeout: Timeout = Timeout(5.seconds)

  var historyReaderOpt: Option[EncryHistory] = None
  var modifiersRequestCache: Map[String, NodeViewModifier] = Map.empty
  var chainSynced: Boolean = false

  val downloadedModifiersValidator: ActorRef = context.system
    .actorOf(DownloadedModifiersValidator.props(settings, nodeViewHolderRef, peersKeeper, self, memoryPoolRef)
      .withDispatcher("Downloaded-Modifiers-Validator-dispatcher"))

  val deliveryManager: ActorRef = context.actorOf(
    DeliveryManager.props(influxRef, nodeViewHolderRef, networkController, settings,
      memoryPoolRef, self, downloadedModifiersValidator)
      .withDispatcher("delivery-manager-dispatcher"), "deliveryManager")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false)
  }

  override def receive: Receive = awaitingHistoryCycle

  def awaitingHistoryCycle: Receive = {
    case ChangedHistory(reader: EncryHistory) =>
      logger.info(s"get history: $reader from $sender")
      deliveryManager ! UpdatedHistory(reader)
      downloadedModifiersValidator ! UpdatedHistory(reader)
      context.become(workingCycle(reader))
    case msg@RegisterMessagesHandler(_, _) => networkController ! msg
    case msg => logger.info(s"Nvsh got strange message: $msg during history awaiting.")
  }

  def workingCycle(history: EncryHistory): Receive = {
    case msg@InvalidModifiers(_) => deliveryManager ! msg
    case msg@RegisterMessagesHandler(_, _) => networkController ! msg
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
      case SyncInfoNetworkMessage(syncInfo) => Option(history) match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, settings.network.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logger.info(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}.")
          if (!(extensionOpt.nonEmpty || comparison != Younger)) logger.warn("Extension is empty while comparison is younger")
          deliveryManager ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
          peersKeeper ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
      case RequestModifiersNetworkMessage(invData) =>
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
            else Option(history).foreach { reader =>
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
        if (invData._1 == Transaction.modifierTypeId) {
          if (chainSynced) memoryPoolRef ! CompareTransactionsWithUnconfirmed(remote, invData._2.toIndexedSeq)
          else logger.debug(s"Get inv with tx: ${invData._2.map(Algos.encode).mkString(",")}") // do nothing
        }
        else if (invData._1 != Payload.modifierTypeId) {
          logger.debug(s"Got inv message on NodeViewSynchronizer from ${remote.socketAddress} with modifiers of type:" +
            s" ${invData._1}. Size of inv is: ${invData._2.size}. Sending CompareViews to NVH. " +
            s"\nModifiers in inv message are: ${invData._2.map(Algos.encode).mkString(",")}")
          nodeViewHolderRef ! CompareViews(remote, invData._1, invData._2)
        } else peersKeeper ! BanPeer(remote, SentInvForPayload)
      case _ => logger.debug(s"NodeViewSyncronyzer got invalid type of DataFromPeer message!")
    }
    case msg@RequestPeersForFirstSyncInfo =>
      logger.info(s"NodeViewSyncronizer got request from delivery manager to peers keeper for" +
        s" peers for first sync info message. Resending $msg to peers keeper.")
      peersKeeper ! msg
    case msg@ModifiersFromRemote(_) => nodeViewHolderRef ! msg
    case msg@RequestFromLocal(_, _, _) => deliveryManager ! msg
    case msg@DownloadRequest(_, _, _) => deliveryManager ! msg
    case msg@UpdatedPeersCollection(_) => deliveryManager ! msg
    case msg@PeersForSyncInfo(_) =>
      logger.info(s"NodeViewSync got peers for sync info. Sending them to DM.")
      deliveryManager ! msg
    case msg@ConnectionStopped(_) => deliveryManager ! msg
    case msg@RequestForTransactions(_, _, _) => deliveryManager ! msg
    case msg@StartMining => deliveryManager ! msg
    case msg@DisableMining => deliveryManager ! msg
    case msg@BanPeer(_, _) => peersKeeper ! msg
    case msg@AccumulatedPeersStatistic(_) => peersKeeper ! msg
    case msg@SendLocalSyncInfo => peersKeeper ! msg
    case msg@RemovePeerFromBlackList(_) => peersKeeper ! msg
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      deliveryManager ! UpdatedHistory(reader)
      downloadedModifiersValidator ! UpdatedHistory(reader)
      context.become(workingCycle(reader))
    case TxsForNVSH(remote, txs) => sendResponse(
      remote, Transaction.modifierTypeId, txs.map(tx => tx.id -> TransactionProtoSerializer.toProto(tx).toByteArray)
    )
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SemanticallyFailedModification(_, _) =>
    case SyntacticallyFailedModification(_, _) =>
    case PeerFromCli(peer) => peersKeeper ! PeerFromCli(peer)
    case FullBlockChainIsSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainIsSynced
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
    if (chainSynced) {
      logger.debug(s"NVSH is synced. Going to broadcast inv for: ${m.encodedId}")
      peersKeeper ! SendToNetwork(InvNetworkMessage(m.modifierTypeId -> Seq(m.id)), Broadcast)
    }
}

object NodeViewSynchronizer {

  object ReceivableMessages {

    case object SendLocalSyncInfo

    final case class OtherNodeSyncingStatus(remote: ConnectedPeer,
                                            status: encry.consensus.History.HistoryComparisonResult,
                                            extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    final case class RequestFromLocal(source: ConnectedPeer,
                                      modifierTypeId: ModifierTypeId,
                                      modifierIds: Seq[ModifierId])

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
            settings: EncryAppSettings,
            memoryPoolRef: ActorRef,
            dataHolder: ActorRef): Props =
    Props(new NodeViewSynchronizer(influxRef, nodeViewHolderRef, settings, memoryPoolRef, dataHolder))

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

        case _ => 3
      })

}