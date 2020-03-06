package encry.view.mempool

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NetworkRouter.{ModifierFromNetwork, RegisterForTxHandling}
import encry.network.PeersKeeper.BanPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.ValidatedModifierFromNetwork
import encry.utils.NetworkTimeProvider
import encry.view.mempool.IntermediaryMempool.IsChainSynced
import encry.view.mempool.MemoryPool.{RolledBackTransactions, TransactionProcessing, TransactionsForMiner}
import encry.view.mempool.TransactionsValidator.{InvalidModifier, ModifiersForValidating}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

class IntermediaryMempool(settings: EncryAppSettings,
                          networkTimeProvider: NetworkTimeProvider,
                          minerReference: ActorRef,
                          influxReference: Option[ActorRef],
                          networkRouter: ActorRef)
    extends Actor
    with StrictLogging {

  val memoryPool: ActorRef =
    context.actorOf(MemoryPool.props(settings, networkTimeProvider, self, influxReference), name = "mempool")
  val txValidator: ActorRef =
    context.actorOf(TransactionsValidator.props(settings, memoryPool, networkTimeProvider),
                    name = "Transaction-validator")
  val mempoolProcessor: ActorRef =
    context.actorOf(MemoryPoolProcessor.props(settings, networkTimeProvider), name = "mempool-processor")

  override def preStart(): Unit = {
    networkRouter ! RegisterForTxHandling
  }

  override def receive(): Receive = {
    case msg @ InvalidModifier(_)                    => // to nvsh
    case msg @ BanPeer(_, _)                         => // to peersKeeper
    case msg @ ValidatedModifierFromNetwork(_)       => // to influx
    case msg @ TransactionsForMiner(_)               => minerReference   ! msg// to miner
    case msg @ RolledBackTransactions(_)             => memoryPool       ! msg // to mempool
    case msg @ ModifiersForValidating(_, _, _)       => memoryPool       ! msg // to mempool
    case msg @ DataFromPeer(_, _)                    => mempoolProcessor ! msg // to mempool processor
    case msg @ RequestFromLocal(_, _, _)             => // to network
    case msg @ ModifierFromNetwork(_, _, _, _)       => txValidator      ! msg
    case msg @ TransactionProcessing(_)              => mempoolProcessor ! msg // to mempool processor
    case msg @ IsChainSynced(_)                      => mempoolProcessor ! msg
  }
}

object IntermediaryMempool {
  def props(settings: EncryAppSettings,
            networkTimeProvider: NetworkTimeProvider,
            minerReference: ActorRef,
            influxReference: Option[ActorRef],
            networkRouter: ActorRef) =
    Props(new IntermediaryMempool(settings, networkTimeProvider, minerReference, influxReference, networkRouter))

  final case class TransactionsForValidating(tx: Transaction)

  final case class IsChainSynced(info: Boolean)
}
