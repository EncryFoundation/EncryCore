package encry.nvg

import akka.actor.{ Actor, ActorRef, Props }
import akka.routing.BalancingPool
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.BlockAndHeaderInfo
import encry.local.miner.Miner.{ DisableMining, StartMining }
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.Messages.MessageToNetwork.{ BroadcastModifier, RequestFromLocal, ResponseFromLocal, SendSyncInfo }
import encry.network.NetworkController.ReceivableMessages.{ DataFromPeer, RegisterMessagesHandler }
import encry.network.NetworkRouter.{ ModifierFromNetwork, RegisterForModsHandling }
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeersKeeper.BanPeer
import encry.nvg.ModifiersValidator.{ InvalidModifierBytes, ModifierForValidation }
import encry.nvg.NodeViewHolder.UpdateHistoryReader
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.StatsSenderMessage
import encry.utils.NetworkTimeProvider
import encry.view.fast.sync.SnapshotHolder.{
  FastSyncDone,
  HeaderChainIsSynced,
  RequiredManifestHeightAndId,
  TreeChunks
}
import encry.view.history.HistoryReader
import encry.view.mempool.MemoryPool.RolledBackTransactions
import org.encryfoundation.common.network.BasicMessagesRepo.{
  InvNetworkMessage,
  RequestModifiersNetworkMessage,
  SyncInfoNetworkMessage
}
import org.encryfoundation.common.utils.Algos

class IntermediaryNVH(
  settings: EncryAppSettings,
  intermediaryNetwork: ActorRef,
  timeProvider: NetworkTimeProvider,
  influxRef: Option[ActorRef]
) extends Actor
    with StrictLogging {

  intermediaryNetwork ! RegisterMessagesHandler(
    Seq(
      InvNetworkMessage.NetworkMessageTypeID              -> "InvNetworkMessage",
      RequestModifiersNetworkMessage.NetworkMessageTypeID -> "RequestModifiersNetworkMessage",
      SyncInfoNetworkMessage.NetworkMessageTypeID         -> "SyncInfoNetworkMessage"
    ),
    self
  )

  intermediaryNetwork ! RegisterForModsHandling

  val networkMessagesProcessor: ActorRef =
    context.actorOf(NetworkMessagesProcessor.props(settings), name = "Network-messages-processor")
  val nodeViewHolder: ActorRef =
    context.actorOf(NodeViewHolder.props(settings, timeProvider, influxRef), name = "Node-view-holder")
  val modifiersValidatorRouter: ActorRef =
    context.actorOf(
      BalancingPool(5)
        .props(ModifiersValidator.props(nodeViewHolder, settings)),
      name = "Modifiers-validator-router"
    )

  var historyReader: HistoryReader = HistoryReader.empty

  override def receive: Receive = {
    case ModifierFromNetwork(remote, typeId, modifierId, modifierBytes) =>
      logger.info(s"Got modifier ${Algos.encode(modifierId)} of type $typeId from $remote for validation.")
      modifiersValidatorRouter ! ModifierForValidation(historyReader, modifierId, typeId, modifierBytes, remote)
    case msg @ DataFromPeer(_, _) => networkMessagesProcessor ! msg
    case msg @ UpdateHistoryReader(newReader: HistoryReader) =>
      historyReader = newReader
      networkMessagesProcessor ! msg
    case msg @ BanPeer(_, _)                         => intermediaryNetwork ! msg
    case msg @ InvalidModifierBytes(_)               => intermediaryNetwork ! msg
    case msg @ OtherNodeSyncingStatus(_, _, _)       => intermediaryNetwork ! msg
    case msg @ RequestFromLocal(_, _, _)             => intermediaryNetwork ! msg
    case msg @ ResponseFromLocal(_, _, _)            => intermediaryNetwork ! msg
    case msg @ BroadcastModifier(_, _)               => intermediaryNetwork ! msg
    case msg @ SyntacticallyFailedModification(_, _) => intermediaryNetwork ! msg
    case msg @ SendSyncInfo(_)                       => intermediaryNetwork ! msg
    case msg @ RequiredManifestHeightAndId(_, _)     => //+ to fast sync
    case msg @ TreeChunks(_, _)                      => //+ to fast sync
    case msg @ FastSyncDone                          =>
    case msg @ HeaderChainIsSynced                   =>
    case msg @ FullBlockChainIsSynced                => //+ to miner
    case msg @ DisableMining                         => //+ to miner
    case msg @ StartMining                           => //+ to miner
    case msg @ BlockAndHeaderInfo(_, _)              => //+ to data holder
    case msg @ RolledBackTransactions(_)             => //+ to memory pool
    case msg: StatsSenderMessage                     => influxRef.foreach(_ ! msg)
    case msg @ RollbackSucceed(_)                    =>
    case msg @ RollbackFailed(_)                     =>
    case msg @ SemanticallySuccessfulModifier(_) =>
      intermediaryNetwork ! msg
      networkMessagesProcessor ! msg
    case msg @ SemanticallyFailedModification(_, _) => intermediaryNetwork ! msg
  }
}

object IntermediaryNVH {
  def props(
    settings: EncryAppSettings,
    intermediaryNetwork: ActorRef,
    timeProvider: NetworkTimeProvider,
    influxRef: Option[ActorRef]
  ): Props = Props(new IntermediaryNVH(settings, intermediaryNetwork, timeProvider, influxRef))
}