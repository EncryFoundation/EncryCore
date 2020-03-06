package encry.view.mempool

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import cats.syntax.either._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.nvg.nvhg.NodeViewHolder.{SemanticallySuccessfulModifier, SuccessfulTransaction}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder.ReceivableMessages.CompareViews
import encry.view.mempool.MemoryPool.MemoryPoolStateType.NotProcessingNewTransactions
import encry.view.mempool.MemoryPool._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

import scala.collection.IndexedSeq

class MemoryPool(settings: EncryAppSettings,
                 networkTimeProvider: NetworkTimeProvider,
                 intermediaryMempool: ActorRef,
                 influxReference: Option[ActorRef]) extends Actor with StrictLogging {

  import context.dispatcher

  var memoryPool: MemoryPoolStorage = MemoryPoolStorage.empty(settings, networkTimeProvider)

  var canProcessTransactions: Boolean = false

  override def preStart(): Unit = {
    logger.debug(s"Starting MemoryPool. Initializing all schedulers")
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
        canProcessTransactions = false
        context.parent ! TransactionProcessing(canProcessTransactions)
        context.become(disableTransactionsProcessor)
      } else {
        val currentTransactionsNumber: Int = currentNumberOfProcessedTransactions + 1
        logger.debug(s"Current number of processed transactions is OK. Continue to process them..." +
          s" Current number is $currentTransactionsNumber.")
        context.become(continueProcessing(currentTransactionsNumber))
      }

//    case CompareViews(peer, _, transactions) =>
//      val notYetRequestedTransactions: IndexedSeq[ModifierId] = notRequestedYet(transactions.toIndexedSeq)
//      if (notYetRequestedTransactions.nonEmpty) {
//        sender ! RequestFromLocal(peer, Transaction.modifierTypeId, notYetRequestedTransactions)
//        logger.debug(s"MemoryPool got inv message with ${transactions.size} ids." +
//          s" Not yet requested ids size is ${notYetRequestedTransactions.size}.")
//      } else logger.debug(s"MemoryPool got inv message with ${transactions.size} ids." +
//        s" There are no not yet requested ids.")

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
        canProcessTransactions = false
        context.parent ! TransactionProcessing(canProcessTransactions)
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
      canProcessTransactions = true
      context.parent ! TransactionProcessing(canProcessTransactions)
      context.become(continueProcessing(currentNumberOfProcessedTransactions = 0))

    case SemanticallySuccessfulModifier(_) =>
      logger.debug(s"MemoryPool got SemanticallySuccessfulModifier with non block modifier" +
        s"while $state. Do nothing in this case.")

    case SendTransactionsToMiner =>
      val (newMemoryPool: MemoryPoolStorage, transactionsForMiner: Seq[Transaction]) =
        memoryPool.getTransactionsForMiner
      memoryPool = newMemoryPool
      intermediaryMempool ! TransactionsForMiner(transactionsForMiner)
      logger.debug(s"MemoryPool got SendTransactionsToMiner. Size of transactions for miner ${transactionsForMiner.size}." +
        s" New pool size is ${memoryPool.size}. Ids ${transactionsForMiner.map(_.encodedId)}")

    case RemoveExpiredFromPool =>
      memoryPool = memoryPool.filter(memoryPool.isExpired)
      logger.debug(s"MemoryPool got RemoveExpiredFromPool message. After cleaning pool size is: ${memoryPool.size}.")

//    case RequestModifiersForTransactions(remote, ids) =>
//      val modifiersIds: Seq[Transaction] = ids
//        .map(Algos.encode)
//        .collect { case if memorascyPool.contains(id) => memoryPool.get(id) }
//        .flatten
//      sender() ! RequestedModifiersForRemote(remote, modifiersIds)
//      logger.debug(s"MemoryPool got request modifiers message. Number of requested ids is ${ids.size}." +
//        s" Number of sent transactions is ${modifiersIds.size}. Request was from $remote.")

    case message => logger.debug(s"MemoryPool got unhandled message $message.")
  }
}

object MemoryPool {

  final case class NewTransaction(tx: Transaction) extends AnyVal

  final case class RolledBackTransactions(txs: IndexedSeq[Transaction]) extends AnyVal

  final case class TransactionsForMiner(txs: Seq[Transaction]) extends AnyVal

  case object SendTransactionsToMiner

  case class TransactionProcessing(info: Boolean)

  case object RemoveExpiredFromPool

  case object StopTransactionsValidation

  case object StartTransactionsValidation

  sealed trait MemoryPoolStateType

  object MemoryPoolStateType {

    case object ProcessingNewTransaction extends MemoryPoolStateType

    case object NotProcessingNewTransactions extends MemoryPoolStateType

  }

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider,
            intermediaryMempool: ActorRef,
            influx: Option[ActorRef]): Props =
    Props(new MemoryPool(settings, ntp, intermediaryMempool, influx))

  class MemoryPoolPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case RemoveExpiredFromPool | SendTransactionsToMiner => 0
        case NewTransaction(_) => 1
        case otherwise => 2
      })

}