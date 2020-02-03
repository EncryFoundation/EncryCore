package encry.view.mempool

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.google.common.base.Charsets
import com.google.common.hash.{BloomFilter, Funnels}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages.{RequestFromLocal, SemanticallySuccessfulModifier, SuccessfulTransaction}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.{EncryAppSettings, MemoryPoolSettings}
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import encry.view.mempool.MemoryPool._
import org.encryfoundation.common.modifiers.history.Block

import scala.concurrent.duration._
import scala.collection.IndexedSeq
import cats.syntax.either._
import encry.EncryApp.system
import encry.view.NodeViewHolder.ReceivableMessages.CompareViews
import encry.view.mempool.MemoryPool.MemoryPoolStateType.NotProcessingNewTransactions

import scala.concurrent.duration.FiniteDuration

class MemoryPool(settings: EncryAppSettings,
                 networkTimeProvider: NetworkTimeProvider,
                 minerReference: ActorRef,
                 influxReference: Option[ActorRef]) extends Actor with StrictLogging {

  import context.dispatcher

  var memoryPool: MemoryPoolStorage = MemoryPoolStorage.empty(settings, networkTimeProvider)

  var bloomFilterForTransactionsIds: BloomFilter[String] = initBloomFilter

  override def preStart(): Unit = {
    logger.debug(s"Starting MemoryPool. Initializing all schedulers")
    context.system.eventStream.subscribe(self, classOf[NewTransaction])
    context.system.scheduler.schedule(
      settings.mempool.bloomFilterCleanupInterval,
      settings.mempool.bloomFilterCleanupInterval, self, CleanupBloomFilter)
    context.system.scheduler.schedule(
      settings.mempool.cleanupInterval,
      settings.mempool.cleanupInterval, self, RemoveExpiredFromPool)
    context.system.scheduler.schedule(
      settings.mempool.txSendingInterval,
      settings.mempool.txSendingInterval, self, SendTransactionsToMiner)
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
  }

  override def receive: Receive = continueProcessing(currentNumberOfProcessedTransactions = 0)

  def continueProcessing(currentNumberOfProcessedTransactions: Int): Receive =
    transactionsProcessor(currentNumberOfProcessedTransactions)
      .orElse(auxiliaryReceive(MemoryPoolStateType.ProcessingNewTransaction))

  def disableTransactionsProcessor: Receive = auxiliaryReceive(MemoryPoolStateType.NotProcessingNewTransactions)

  def transactionsProcessor(currentNumberOfProcessedTransactions: Int): Receive = {
    case NewTransaction(transaction) =>
      val (newMemoryPool: MemoryPoolStorage, validatedTransaction: Option[Transaction]) =
        memoryPool.validateTransaction(transaction)
      memoryPool = newMemoryPool
      validatedTransaction.foreach(tx => context.system.eventStream.publish(SuccessfulTransaction(tx)))
      logger.debug(s"MemoryPool got new transactions from remote. New pool size is ${memoryPool.size}.")
      if (currentNumberOfProcessedTransactions > settings.mempool.transactionsLimit) {
        logger.debug(s"MemoryPool has its limit of processed transactions. " +
          s"Transit to 'disableTransactionsProcessor' state." +
          s"Current number of processed transactions is $currentNumberOfProcessedTransactions.")
        Either.catchNonFatal(context.system.actorSelection("/user/nodeViewSynchronizer") ! StopTransactionsValidation)
        context.become(disableTransactionsProcessor)
      } else {
        val currentTransactionsNumber: Int = currentNumberOfProcessedTransactions + 1
        logger.debug(s"Current number of processed transactions is OK. Continue to process them..." +
          s" Current number is $currentTransactionsNumber.")
        context.become(continueProcessing(currentTransactionsNumber))
      }

    case CompareViews(peer, _, transactions) =>
      val notYetRequestedTransactions: IndexedSeq[ModifierId] = notRequestedYet(transactions.toIndexedSeq)
      if (notYetRequestedTransactions.nonEmpty) {
        sender ! RequestFromLocal(peer, Transaction.modifierTypeId, notYetRequestedTransactions)
        logger.debug(s"MemoryPool got inv message with ${transactions.size} ids." +
          s" Not yet requested ids size is ${notYetRequestedTransactions.size}.")
      } else logger.debug(s"MemoryPool got inv message with ${transactions.size} ids." +
        s" There are no not yet requested ids.")

    case RolledBackTransactions(transactions) =>
      val (newMemoryPool: MemoryPoolStorage, validatedTransactions: Seq[Transaction]) =
        memoryPool.validateTransactions(transactions)
      memoryPool = newMemoryPool
      logger.debug(s"MemoryPool got rolled back transactions. New pool size is ${memoryPool.size}." +
        s"Number of rolled back transactions is ${validatedTransactions.size}.")
      if (currentNumberOfProcessedTransactions > settings.mempool.transactionsLimit) {
        logger.debug(s"MemoryPool has its limit of processed transactions. " +
          s"Transit to 'disableTransactionsProcessor' state." +
          s"Current number of processed transactions is $currentNumberOfProcessedTransactions.")
        Either.catchNonFatal(context.system.actorSelection("/user/nodeViewSynchronizer") ! StopTransactionsValidation)
        context.become(disableTransactionsProcessor)
      } else {
        val currentTransactionsNumber: Int = currentNumberOfProcessedTransactions + validatedTransactions.size
        logger.debug(s"Current number of processed transactions is OK. Continue to process them..." +
          s" Current number is $currentTransactionsNumber.")
        context.become(continueProcessing(currentTransactionsNumber))
      }
  }

  def auxiliaryReceive(state: MemoryPoolStateType): Receive = {
    case SemanticallySuccessfulModifier(modifier) if modifier.modifierTypeId == Block.modifierTypeId =>
      logger.debug(s"MemoryPool got SemanticallySuccessfulModifier with new block while $state." +
        s"Transit to a transactionsProcessor state.")
      if (state == NotProcessingNewTransactions)
        Either.catchNonFatal(context.system.actorSelection("/user/nodeViewSynchronizer") ! StartTransactionsValidation)
      context.become(continueProcessing(currentNumberOfProcessedTransactions = 0))

    case SemanticallySuccessfulModifier(_) =>
      logger.debug(s"MemoryPool got SemanticallySuccessfulModifier with non block modifier" +
        s"while $state. Do nothing in this case.")

    case CleanupBloomFilter =>
      bloomFilterForTransactionsIds = initBloomFilter

    case SendTransactionsToMiner =>
      val (newMemoryPool: MemoryPoolStorage, transactionsForMiner: Seq[Transaction]) =
        memoryPool.getTransactionsForMiner
      memoryPool = newMemoryPool
      minerReference ! TransactionsForMiner(transactionsForMiner)
      logger.debug(s"MemoryPool got SendTransactionsToMiner. Size of transactions for miner ${transactionsForMiner.size}." +
        s" New pool size is ${memoryPool.size}. Ids ${transactionsForMiner.map(_.encodedId)}")

    case RemoveExpiredFromPool =>
      memoryPool = memoryPool.filter(memoryPool.isExpired)
      logger.debug(s"MemoryPool got RemoveExpiredFromPool message. After cleaning pool size is: ${memoryPool.size}.")

    case RequestModifiersForTransactions(remote, ids) =>
      val modifiersIds: Seq[Transaction] = ids
        .map(Algos.encode)
        .collect { case id if memoryPool.contains(id) => memoryPool.get(id) }
        .flatten
      sender() ! RequestedModifiersForRemote(remote, modifiersIds)
      logger.debug(s"MemoryPool got request modifiers message. Number of requested ids is ${ids.size}." +
        s" Number of sent transactions is ${modifiersIds.size}. Request was from $remote.")

    case message => logger.debug(s"MemoryPool got unhandled message $message.")
  }

  def initBloomFilter: BloomFilter[String] = BloomFilter.create(
    Funnels.stringFunnel(Charsets.UTF_8),
    settings.mempool.bloomFilterCapacity,
    settings.mempool.bloomFilterFailureProbability
  )

  def notRequestedYet(ids: IndexedSeq[ModifierId]): IndexedSeq[ModifierId] = ids.collect {
    case id: ModifierId if !bloomFilterForTransactionsIds.mightContain(Algos.encode(id)) =>
      bloomFilterForTransactionsIds.put(Algos.encode(id))
      id
  }
}

object MemoryPool {

  final case class NewTransaction(tx: Transaction) extends AnyVal

  final case class RolledBackTransactions(txs: IndexedSeq[Transaction]) extends AnyVal

  final case class TransactionsForMiner(txs: Seq[Transaction]) extends AnyVal

  final case class RequestModifiersForTransactions(peer: ConnectedPeer, txsIds: Seq[ModifierId])

  final case class RequestedModifiersForRemote(peer: ConnectedPeer, txs: Seq[Transaction])

  case object SendTransactionsToMiner

  case object RemoveExpiredFromPool

  case object CleanupBloomFilter

  case object StopTransactionsValidation

  case object StartTransactionsValidation

  sealed trait MemoryPoolStateType

  object MemoryPoolStateType {

    case object ProcessingNewTransaction extends MemoryPoolStateType

    case object NotProcessingNewTransactions extends MemoryPoolStateType

  }

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider,
            minerRef: ActorRef,
            influx: Option[ActorRef]): Props =
    Props(new MemoryPool(settings, ntp, minerRef, influx))

  class MemoryPoolPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case RemoveExpiredFromPool | CleanupBloomFilter | SendTransactionsToMiner => 0
        case NewTransaction(_) => 1
        case CompareViews(_, _, _) | RequestModifiersForTransactions(_, _) => 2
        case otherwise => 3
      })

}