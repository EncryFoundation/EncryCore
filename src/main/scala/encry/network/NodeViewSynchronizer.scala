package encry.network

import HeaderProto.HeaderProtoMessage
import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.util.Timeout
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus._
import encry.local.miner.Miner.{DisableMining, ClIMiner, StartMining}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DownloadedModifiersValidator.InvalidModifier
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
import encry.view.mempool.MemoryPool._
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionProtoSerializer}
import org.encryfoundation.common.network.BasicMessagesRepo._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scala.concurrent.duration._
import encry.network.ModifiersToNetworkUtils._
import encry.view.NodeViewHolder.DownloadRequest
import encry.view.NodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.fast.sync.SnapshotHolder
import encry.view.fast.sync.SnapshotHolder.{FastSyncDone, HeaderChainIsSynced, RequiredManifestHeightAndId, TreeChunks, UpdateSnapshot}
import scala.util.Try

class NodeViewSynchronizer(influxRef: Option[ActorRef],
                           nodeViewHolderRef: ActorRef,
                           settings: EncryAppSettings,
                           memoryPoolRef: ActorRef,
                           dataHolder: ActorRef) extends Actor with StrictLogging {

  val peersKeeper: ActorRef = context.system.actorOf(PeersKeeper.props(settings, self, dataHolder)
    .withDispatcher("peers-keeper-dispatcher"), "PeersKeeper")


  val networkController: ActorRef = context.system.actorOf(NetworkController.props(settings.network, peersKeeper, self)
    .withDispatcher("network-dispatcher"), "NetworkController")

  val snapshotHolder: ActorRef = context.system.actorOf(SnapshotHolder.props(settings, networkController, nodeViewHolderRef, self)
  .withDispatcher("snapshot-holder-dispatcher"), "snapshotHolder")

  networkController ! RegisterMessagesHandler(Seq(
    InvNetworkMessage.NetworkMessageTypeID -> "InvNetworkMessage",
    RequestModifiersNetworkMessage.NetworkMessageTypeID -> "RequestModifiersNetworkMessage",
    SyncInfoNetworkMessage.NetworkMessageTypeID -> "SyncInfoNetworkMessage"
  ), self)

  implicit val timeout: Timeout = Timeout(5.seconds)

  var historyReaderOpt: Option[History] = None
  var modifiersRequestCache: Map[String, Array[Byte]] = Map.empty
  var chainSynced: Boolean = false

  var canProcessTransactions: Boolean = true

  val downloadedModifiersValidator: ActorRef = context.system
    .actorOf(DownloadedModifiersValidator.props(settings.constants.ModifierIdSize, nodeViewHolderRef,
      peersKeeper, self, memoryPoolRef, influxRef, settings)
      .withDispatcher("Downloaded-Modifiers-Validator-dispatcher"), "DownloadedModifiersValidator")

  val deliveryManager: ActorRef = context.actorOf(
    DeliveryManager.props(influxRef, nodeViewHolderRef, networkController, memoryPoolRef, self,
      downloadedModifiersValidator, settings)
      .withDispatcher("delivery-manager-dispatcher"), "DeliveryManager")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    context.system.eventStream.subscribe(self, classOf[ClIMiner])
    context.system.eventStream.subscribe(self, classOf[CLIPeer])
    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false)
  }

  override def receive: Receive = awaitingHistoryCycle

  def awaitingHistoryCycle: Receive = {
    case msg@ChangedHistory(reader: History) =>
      logger.info(s"get history: $reader from $sender")
      deliveryManager ! UpdatedHistory(reader)
      snapshotHolder ! msg
      downloadedModifiersValidator ! UpdatedHistory(reader)
      context.become(workingCycle(reader))
    case msg@RegisterMessagesHandler(_, _) => networkController ! msg
    case msg => logger.info(s"Nvsh got strange message: $msg during history awaiting.")
  }

  def workingCycle(history: History): Receive = {
    case msg@InvalidModifier(_) => deliveryManager ! msg
    case msg@RegisterMessagesHandler(_, _) => networkController ! msg
    case SemanticallySuccessfulModifier(mod) => mod match {
      case block: Block if chainSynced =>
        broadcastModifierInv(block.header)
        broadcastModifierInv(block.payload)
        modifiersRequestCache = Map(
          Algos.encode(block.id)         -> toProto(block.header),
          Algos.encode(block.payload.id) -> toProto(block.payload)
        )
      case tx: Transaction => broadcastModifierInv(tx)
      case _ => //Do nothing
    }
    case DataFromPeer(message, remote) => message match {
      case SyncInfoNetworkMessage(syncInfo) => Option(history) match {
        case Some(historyReader) =>
          val ext: Seq[ModifierId] = historyReader.continuationIds(syncInfo, settings.network.syncPacketLength)
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          logger.info(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}.")
          if (!(ext.nonEmpty || comparison != Younger)) logger.warn("Extension is empty while comparison is younger")
          deliveryManager ! OtherNodeSyncingStatus(remote, comparison, Some(ext.map(h => Header.modifierTypeId -> h)))
          peersKeeper ! OtherNodeSyncingStatus(remote, comparison, Some(ext.map(h => Header.modifierTypeId -> h)))
        case _ =>
      }
      case RequestModifiersNetworkMessage((typeId, requestedIds)) if chainSynced || settings.node.offlineGeneration =>
        val modifiersFromCache: Map[ModifierId, Array[Byte]] = requestedIds
          .flatMap(id => modifiersRequestCache
            .get(Algos.encode(id))
            .map(id -> _))
          .toMap
        if (modifiersFromCache.nonEmpty) remote.handlerRef ! ModifiersNetworkMessage(typeId -> modifiersFromCache)
        val unrequestedModifiers: Seq[ModifierId] = requestedIds.filterNot(modifiersFromCache.contains)

        if (unrequestedModifiers.nonEmpty) typeId match {
          case Transaction.modifierTypeId =>
            memoryPoolRef ! RequestModifiersForTransactions(remote, unrequestedModifiers)
          case Payload.modifierTypeId =>
            getModsForRemote(unrequestedModifiers).foreach(_.foreach {
              case (id, bytes) =>
                remote.handlerRef ! ModifiersNetworkMessage(typeId -> Map(id -> bytes))
            })
          case tId => getModsForRemote(unrequestedModifiers).foreach { modifiers =>
            modifiers.foreach(k =>
              logger.info(s"Response to ${remote.socketAddress} header ${
                Try(HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(k._2)))
              }")
            )
            remote.handlerRef ! ModifiersNetworkMessage(tId -> modifiers)
          }
        }

        def getModsForRemote(ids: Seq[ModifierId]): Option[Map[ModifierId, Array[Byte]]] = Option(history)
          .map { historyStorage =>
            val modifiers: Map[ModifierId, Array[Byte]] = unrequestedModifiers
              .view
              .map(id => id -> historyStorage.modifierBytesById(id))
              .collect { case (id, mod) if mod.isDefined => id -> mod.get}
              .toMap
            logger.debug(s"Send response to $remote with ${modifiers.size} modifiers of type $typeId")
            modifiers
          }

      case RequestModifiersNetworkMessage(requestedIds) =>
        logger.info(s"Request from $remote for ${requestedIds._2.size} modifiers discarded cause to chain isn't synced")

      case InvNetworkMessage(modifiersType, ids) =>
        if (modifiersType != Transaction.modifierTypeId)
          logger.debug(s"Got inv message on NodeViewSynchronizer from ${remote.socketAddress} with modifiers of type:" +
            s" $modifiersType. Size of inv is: ${ids.size}. Sending CompareViews to NVH. " +
            s"\nModifiers in inv message are: ${ids.map(Algos.encode).mkString(",")}")

        if (modifiersType == Transaction.modifierTypeId && chainSynced && canProcessTransactions)
          memoryPoolRef ! CompareViews(remote, modifiersType, ids)
        else if (modifiersType == Transaction.modifierTypeId)
          logger.debug(s"Get inv with tx: ${ids.map(Algos.encode).mkString(",")}, but " +
            s"chainSynced is $chainSynced and canProcessTransactions is $canProcessTransactions.")
        else if (modifiersType == Payload.modifierTypeId && !history.isFullChainSynced)
          logger.info(s"Got inv message with payloads: ${ids.map(Algos.encode).mkString(",")}. " +
            s"But full chain is not synced. Ignore them.")
        else nodeViewHolderRef ! CompareViews(remote, modifiersType, ids)

      case _ => logger.debug(s"NodeViewSyncronyzer got invalid type of DataFromPeer message!")
    }
    case msg@RequestPeersForFirstSyncInfo =>
      logger.info(s"NodeViewSyncronizer got request from delivery manager to peers keeper for" +
        s" peers for first sync info message. Resending $msg to peers keeper.")
      peersKeeper ! msg
    case msg@RequestFromLocal(_, _, _) => deliveryManager ! msg
    case msg@DownloadRequest(_, _, _) => deliveryManager ! msg
    case msg@UpdatedPeersCollection(_) => deliveryManager ! msg
    case msg@PeersForSyncInfo(_) =>
      logger.info(s"NodeViewSync got peers for sync info. Sending them to DM.")
      deliveryManager ! msg
    case msg@TreeChunks(l, b) => snapshotHolder ! msg
    case msg@ConnectionStopped(_) => deliveryManager ! msg
    case msg@StartMining => deliveryManager ! msg
    case msg@DisableMining => deliveryManager ! msg
    case msg@BanPeer(_, _) => peersKeeper ! msg
    case msg@AccumulatedPeersStatistic(_) => peersKeeper ! msg
    case msg@SendLocalSyncInfo => peersKeeper ! msg
    case msg@RemovePeerFromBlackList(_) => peersKeeper ! msg
    case msg@RequiredManifestHeightAndId(_, _) => snapshotHolder ! msg
    case msg@SendToNetwork(_, _) =>
      logger.info(s"NVSH got SendToNetwork")
      peersKeeper ! msg
    case msg@HeaderChainIsSynced =>
      snapshotHolder ! msg
    case msg@UpdateSnapshot(_, _) => snapshotHolder ! msg
    case msg@FastSyncDone => snapshotHolder ! FastSyncDone
    case ChangedHistory(reader: History@unchecked) if reader.isInstanceOf[History] =>
      deliveryManager ! UpdatedHistory(reader)
      downloadedModifiersValidator ! UpdatedHistory(reader)
      context.become(workingCycle(reader))
    case RequestedModifiersForRemote(remote, txs) => sendResponse(
      remote, Transaction.modifierTypeId, txs.map(tx => tx.id -> TransactionProtoSerializer.toProto(tx).toByteArray)
    )
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SemanticallyFailedModification(_, _) =>
    case SyntacticallyFailedModification(_, _) =>
    case msg@PeerFromCli(peer) => peersKeeper ! msg
    case FullBlockChainIsSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainIsSynced
      peersKeeper ! FullBlockChainIsSynced
      if (!settings.snapshotSettings.enableFastSynchronization) snapshotHolder ! FullBlockChainIsSynced
    case StopTransactionsValidation =>
      deliveryManager ! StopTransactionsValidation
      canProcessTransactions = false
    case StartTransactionsValidation =>
      deliveryManager ! StartTransactionsValidation
      canProcessTransactions = true
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
            s" Mods length: ${modifiersBytes.map(_._2.length).mkString(",")}" +
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
                                            status: encry.consensus.HistoryConsensus.HistoryComparisonResult,
                                            extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    final case class RequestFromLocal(source: ConnectedPeer,
                                      modifierTypeId: ModifierTypeId,
                                      modifierIds: Seq[ModifierId])
    sealed trait CLIPeer

    final case class PeerFromCli(address: InetSocketAddress) extends CLIPeer

    final case class RemovePeerFromBlackList(address: InetSocketAddress) extends CLIPeer

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory(reader: History) extends NodeViewChange

    final case class UpdatedHistory(history: History) extends AnyVal

    case class ChangedState(reader: UtxoState) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class SyntacticallyFailedModification(modifier: PersistentNodeViewModifier, errors: List[ModifierApplyError])
      extends ModificationOutcome

    case class SemanticallyFailedModification(modifier: PersistentNodeViewModifier, errors: List[ModifierApplyError])
      extends ModificationOutcome

    case class SuccessfulTransaction(transaction: Transaction) extends ModificationOutcome

    case class SemanticallySuccessfulModifier(modifier: PersistentNodeViewModifier) extends ModificationOutcome

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