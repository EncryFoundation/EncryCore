package encry.nvg

import akka.actor.{ Actor, ActorRef }
import akka.routing.BalancingPool
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.BlockAndHeaderInfo
import encry.local.miner.Miner.{ DisableMining, StartMining }
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DownloadedModifiersValidator.{ InvalidModifier, ModifiersForValidating }
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{
  RollbackFailed,
  RollbackSucceed,
  SemanticallyFailedModification,
  SemanticallySuccessfulModifier
}
import encry.network.PeersKeeper.BanPeer
import encry.nvg.ModifiersValidator.ModifierForValidation
import encry.nvg.NodeViewHolder.{ DownloadRequest, UpdateHistoryReader }
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
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

class IntermediaryNVH(
  settings: EncryAppSettings,
  intermediaryNetwork: ActorRef,
  timeProvider: NetworkTimeProvider,
  influxRef: Option[ActorRef]
) extends Actor
    with StrictLogging {

  val networkMessagesProcessor: ActorRef =
    context.actorOf(NetworkMessagesProcessor.props, name = "Network-messages-processor")
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
    case ModifiersForValidating(remote, typeId, modifiers) =>
      logger.info(s"Got ${modifiers.size} modifiers of type $typeId for validation.")
      modifiers.foreach {
        case (id: ModifierId, bytes: Array[Byte]) =>
          modifiersValidatorRouter ! ModifierForValidation(historyReader, id, typeId, bytes, remote)
      }
    case msg @ DataFromPeer(_, _) => networkMessagesProcessor ! msg
    case UpdateHistoryReader(newReader: HistoryReader) =>
      historyReader = newReader
      networkMessagesProcessor ! newReader
    case msg @ BanPeer(_, _)                        => networkMessagesProcessor ! msg
    case msg @ InvalidModifier(_)                   => networkMessagesProcessor ! msg
    case msg @ FastSyncDone                         => networkMessagesProcessor ! msg
    case msg @ DownloadRequest(_, _, _)             => networkMessagesProcessor ! msg
    case msg @ RequiredManifestHeightAndId(_, _)    => //+ to fast sync
    case msg @ TreeChunks(_, _)                     => //+ to fast sync
    case msg @ HeaderChainIsSynced                  => networkMessagesProcessor ! msg
    case msg @ FullBlockChainIsSynced               => networkMessagesProcessor ! msg //+ to miner
    case msg @ DisableMining                        => //+ to miner
    case msg @ StartMining                          => //+ to miner
    case msg @ BlockAndHeaderInfo(_, _)             => //+ to data holder
    case msg @ RolledBackTransactions(_)            => //+ to memory pool
    case msg: StatsSenderMessage                    => //+ to stats sender
    case msg @ RollbackSucceed(_)                   =>
    case msg @ RollbackFailed(_)                    =>
    case msg @ SemanticallySuccessfulModifier(_)    =>
    case msg @ SemanticallyFailedModification(_, _) =>
  }
}

object IntermediaryNVH {

}
