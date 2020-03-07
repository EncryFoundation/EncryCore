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
import encry.view.mempool.TransactionsValidator.{InvalidTransaction, ModifiersForValidating}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

class IntermediaryMempool(settings: EncryAppSettings,
                          networkTimeProvider: NetworkTimeProvider,
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
    case msg: InvalidTransaction             => networkRouter ! msg
    case msg: BanPeer                        => networkRouter ! msg
    case msg: ValidatedModifierFromNetwork   => // to influx
    case msg: RolledBackTransactions         => memoryPool       ! msg // to mempool
    case msg: ModifiersForValidating         => memoryPool       ! msg // to mempool
    case msg: DataFromPeer                   => mempoolProcessor ! msg // to mempool processor
    case msg: RequestFromLocal               => networkRouter    ! msg
    case msg: ModifierFromNetwork            => txValidator      ! msg
    case msg: TransactionProcessing          => mempoolProcessor ! msg // to mempool processor
    case msg: IsChainSynced                  => mempoolProcessor ! msg
  }
}

object IntermediaryMempool {

  def props(settings: EncryAppSettings,
            networkTimeProvider: NetworkTimeProvider,
            influxReference: Option[ActorRef],
            networkRouter: ActorRef): Props =
    Props(new IntermediaryMempool(settings, networkTimeProvider, influxReference, networkRouter))

  final case class TransactionsForValidating(tx: Transaction)

  final case class IsChainSynced(info: Boolean)
}
