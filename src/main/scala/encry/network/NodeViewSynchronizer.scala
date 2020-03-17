package encry.network

import HeaderProto.HeaderProtoMessage
import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus._
import encry.local.miner.Miner.{MinerMiningCommands, DisableMining, StartMining}
import encry.mpg.MemoryPool._
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper._
import encry.network.PrioritiesCalculator.AccumulatedPeersStatistic
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.Utils._
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.history.History
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

import scala.concurrent.duration._
import encry.network.ModifiersToNetworkUtils._
import encry.nvg.NodeViewHolder.{NodeViewChange, NodeViewHolderEvent, SemanticallySuccessfulModifier, SuccessfulTransaction}
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}

import scala.util.Try

//class NodeViewSynchronizer(influxRef: Option[ActorRef],
//                           nodeViewHolderRef: ActorRef,
//                           settings: EncryAppSettings,
//                           memoryPoolRef: ActorRef,
//                           dataHolder: ActorRef) extends Actor with StrictLogging {
//
//  val peersKeeper: ActorRef = context.system.actorOf(PeersKeeper.props(settings, self, dataHolder)
//    .withDispatcher("peers-keeper-dispatcher"), "PeersKeeper")
//
//
//  val networkController: ActorRef = context.system.actorOf(NetworkController.props(settings.network, peersKeeper, self)
//    .withDispatcher("network-dispatcher"), "NetworkController")
//
//  val snapshotHolder: ActorRef = context.system.actorOf(SnapshotHolder.props(settings, networkController, nodeViewHolderRef, self)
//  .withDispatcher("snapshot-holder-dispatcher"), "snapshotHolder")
//
//  networkController ! RegisterMessagesHandler(Seq(
//    InvNetworkMessage.NetworkMessageTypeID -> "InvNetworkMessage",
//    RequestModifiersNetworkMessage.NetworkMessageTypeID -> "RequestModifiersNetworkMessage",
//    SyncInfoNetworkMessage.NetworkMessageTypeID -> "SyncInfoNetworkMessage"
//  ), self)
//
//  implicit val timeout: Timeout = Timeout(5.seconds)
//
//  var historyReaderOpt: Option[History] = None
//  var modifiersRequestCache: Map[String, Array[Byte]] = Map.empty
//  var chainSynced: Boolean = false
//
//  var canProcessTransactions: Boolean = true
//
//  val downloadedModifiersValidator: ActorRef = context.system
//    .actorOf(DownloadedModifiersValidator.props(settings.constants.ModifierIdSize, nodeViewHolderRef,
//      peersKeeper, self, memoryPoolRef, influxRef, settings)
//      .withDispatcher("Downloaded-Modifiers-Validator-dispatcher"), "DownloadedModifiersValidator")
//
//  val deliveryManager: ActorRef = context.actorOf(
//    DeliveryManager.props(influxRef, nodeViewHolderRef, networkController, memoryPoolRef, self,
//      downloadedModifiersValidator, settings)
//      .withDispatcher("delivery-manager-dispatcher"), "DeliveryManager")
//
//  override def preStart(): Unit = {
//    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
//    context.system.eventStream.subscribe(self, classOf[ClIMiner])
//    context.system.eventStream.subscribe(self, classOf[CLIPeer])
//    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false)
//  }
//
//  override def receive: Receive = awaitingHistoryCycle
//
//  def awaitingHistoryCycle: Receive = {
//    case msg@ChangedHistory(reader: History) =>
//      logger.info(s"get history: $reader from $sender")
//      deliveryManager ! UpdatedHistory(reader)
//      snapshotHolder ! msg
//      downloadedModifiersValidator ! UpdatedHistory(reader)
//      context.become(workingCycle(reader))
//    case msg@RegisterMessagesHandler(_, _) => networkController ! msg
//    case msg => logger.info(s"Nvsh got strange message: $msg during history awaiting.")
//  }
//
//  def workingCycle(history: History): Receive = {
//    case msg@InvalidModifier(_) => deliveryManager ! msg
//    case msg@RegisterMessagesHandler(_, _) => networkController ! msg
//    case SemanticallySuccessfulModifier(mod) => mod match {
//      case block: Block if chainSynced =>
//        broadcastModifierInv(block.header)
//        broadcastModifierInv(block.payload)
//        modifiersRequestCache = Map(
//          Algos.encode(block.id)         -> toProto(block.header),
//          Algos.encode(block.payload.id) -> toProto(block.payload)
//        )
//      case tx: Transaction => broadcastModifierInv(tx)
//      case _ => //Do nothing
//    }
//    case DataFromPeer(message, remote) => message match {
//
//      case RequestModifiersNetworkMessage((typeId, requestedIds)) if chainSynced || settings.node.offlineGeneration =>
//        val modifiersFromCache: Map[ModifierId, Array[Byte]] = requestedIds
//          .flatMap(id => modifiersRequestCache
//            .get(Algos.encode(id))
//            .map(id -> _))
//          .toMap
//        if (modifiersFromCache.nonEmpty) remote.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersFromCache)
//        val unrequestedModifiers: Seq[ModifierId] = requestedIds.filterNot(modifiersFromCache.contains)
//
//        if (unrequestedModifiers.nonEmpty) typeId match {
//          case Transaction.modifierTypeId =>
//            memoryPoolRef ! RequestModifiersForTransactions(remote, unrequestedModifiers)
//        }
//
//      case RequestModifiersNetworkMessage(requestedIds) =>
//        logger.info(s"Request from $remote for ${requestedIds._2.size} modifiers discarded cause to chain isn't synced")
//
//      case InvNetworkMessage(invData) if invData._1 == Transaction.modifierTypeId && chainSynced && canProcessTransactions =>
//        memoryPoolRef ! CompareViews(remote, invData._1, invData._2)
//      case InvNetworkMessage(invData) if invData._1 == Transaction.modifierTypeId =>
//        logger.debug(s"Get inv with tx: ${invData._2.map(Algos.encode).mkString(",")}, but " +
//          s"chainSynced is $chainSynced and canProcessTransactions is $canProcessTransactions.")
//
//      case _ => logger.debug(s"NodeViewSyncronyzer got invalid type of DataFromPeer message!")
//    }
//    case msg@RequestPeersForFirstSyncInfo =>
//      logger.info(s"NodeViewSyncronizer got request from delivery manager to peers keeper for" +
//        s" peers for first sync info message. Resending $msg to peers keeper.")
//      peersKeeper ! msg
//    case msg@UpdatedPeersCollection(_) => deliveryManager ! msg
//    case msg@PeersForSyncInfo(_) =>
//      logger.info(s"NodeViewSync got peers for sync info. Sending them to DM.")
//      deliveryManager ! msg
//    case msg@TreeChunks(l, b) => snapshotHolder ! msg
//    case msg@ConnectionStopped(_) => deliveryManager ! msg
//    case msg@StartMining => deliveryManager ! msg
//    case msg@DisableMining => deliveryManager ! msg
//    case msg@BanPeer(_, _) => peersKeeper ! msg
//    case msg@AccumulatedPeersStatistic(_) => peersKeeper ! msg
//    case msg@SendLocalSyncInfo => peersKeeper ! msg
//    case msg@RemovePeerFromBlackList(_) => peersKeeper ! msg
//    case msg@RequiredManifestHeightAndId(_, _) => snapshotHolder ! msg
//    case msg@SendToNetwork(_, _) =>
//      logger.info(s"NVSH got SendToNetwork")
//      peersKeeper ! msg
//    case msg@HeaderChainIsSynced =>
//      snapshotHolder ! msg
//    case msg@UpdateSnapshot(_, _) => snapshotHolder ! msg
//    case msg@FastSyncDone => snapshotHolder ! FastSyncDone
//    case ChangedHistory(reader: History@unchecked) if reader.isInstanceOf[History] =>
//      deliveryManager ! UpdatedHistory(reader)
//      downloadedModifiersValidator ! UpdatedHistory(reader)
//      context.become(workingCycle(reader))
//    case RequestedModifiersForRemote(remote, txs) => sendResponse(
//      remote, Transaction.modifierTypeId, txs.map(tx => tx.id -> TransactionProtoSerializer.toProto(tx).toByteArray)
//    )
//    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
//    case SemanticallyFailedModification(_, _) =>
//    case SyntacticallyFailedModification(_, _) =>
//    case msg@PeerFromCli(peer) => peersKeeper ! msg
//    case FullBlockChainIsSynced =>
//      chainSynced = true
//      deliveryManager ! FullBlockChainIsSynced
//      peersKeeper ! FullBlockChainIsSynced
//      if (!settings.snapshotSettings.enableFastSynchronization) snapshotHolder ! FullBlockChainIsSynced
//    case StopTransactionsValidation =>
//      deliveryManager ! StopTransactionsValidation
//      canProcessTransactions = false
//    case StartTransactionsValidation =>
//      deliveryManager ! StartTransactionsValidation
//      canProcessTransactions = true
//    case a: Any => logger.error(s"Strange input(sender: ${sender()}): ${a.getClass}\n" + a)
//  }
//
//  def sendResponse(peer: ConnectedPeer, typeId: ModifierTypeId, modifiersBytes: Seq[(ModifierId, Array[Byte])]): Unit =
//    if (modifiersBytes.nonEmpty) {
//      if (typeId != Transaction.modifierTypeId)
//        logger.debug(s"Sent modifiers to $peer size is: ${modifiersBytes.length}")
//      typeId match {
//        case Header.modifierTypeId =>
//          logger.debug(s"Sent to peer handler for $peer ModfiersNetworkMessage for HEADERS with ${modifiersBytes.size} headers." +
//            s" \n Headers are: ${modifiersBytes.map(x => Algos.encode(x._1)).mkString(",")}.")
//          peer.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersBytes.toMap)
//        case Payload.modifierTypeId =>
//          logger.debug(s"Sent to peer handler for $peer ModfiersNetworkMessage for PAYLOADS with ${modifiersBytes.size} payloads." +
//            s" Mods length: ${modifiersBytes.map(_._2.length).mkString(",")}" +
//            s" \n Payloads are: ${modifiersBytes.map(x => Algos.encode(x._1)).mkString(",")}.")
//          peer.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersBytes.toMap)
//        case Transaction.modifierTypeId =>
//          peer.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersBytes.toMap)
//      }
//    }
//
//  def broadcastModifierInv(m: NodeViewModifier): Unit =
//    if (chainSynced) {
//      logger.debug(s"NVSH is synced. Going to broadcast inv for: ${m.encodedId}")
//      peersKeeper ! SendToNetwork(InvNetworkMessage(m.modifierTypeId -> Seq(m.id)), Broadcast)
//    }
//}

object NodeViewSynchronizer {

  object ReceivableMessages {

    case object SendLocalSyncInfo

    final case class OtherNodeSyncingStatus(remote: InetSocketAddress, status: HistoryComparisonResult)

    sealed trait CLIPeer

    final case class PeerFromCli(address: InetSocketAddress) extends CLIPeer

    final case class RemovePeerFromBlackList(address: InetSocketAddress) extends CLIPeer

    case class ChangedHistory(reader: History) extends NodeViewChange

    final case class UpdatedHistory(history: History) extends AnyVal

    case class ChangedState(reader: UtxoState) extends NodeViewChange

    trait ModificationOutcome extends NodeViewHolderEvent

  }

//  def props(influxRef: Option[ActorRef],
//            nodeViewHolderRef: ActorRef,
//            settings: EncryAppSettings,
//            memoryPoolRef: ActorRef,
//            dataHolder: ActorRef): Props =
//    Props(new NodeViewSynchronizer(influxRef, nodeViewHolderRef, settings, memoryPoolRef, dataHolder))

  class NodeViewSynchronizerPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {

//        case DataFromPeer(msg, _) => msg match {
//          case SyncInfoNetworkMessage(_) => 1
//          case InvNetworkMessage(data) if data._1 != Transaction.modifierTypeId => 1
//          case RequestModifiersNetworkMessage(data) if data._1 != Transaction.modifierTypeId => 2
//          case _ => 4
//        }

        case SemanticallySuccessfulModifier(mod) => mod match {
          case _: Transaction => 4
          case _ => 1
        }

        case StopTransactionsValidation => 2

        case StartTransactionsValidation => 2

        case SuccessfulTransaction(_) => 4

        case PoisonPill => 5

        case _ => 3
      })

}