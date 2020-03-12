package encry.mpg

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.dispatch.{ PriorityGenerator, UnboundedStablePriorityMailbox }
import cats.syntax.either._
import com.google.common.base.Charsets
import com.google.common.hash.{ BloomFilter, Funnels }
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.mpg.MemoryPool.MemoryPoolStateType.NotProcessingNewTransactions
import encry.network.Messages.MessageToNetwork.{ RequestFromLocal, ResponseFromLocal }
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.nvg.NodeViewHolder.{ SemanticallySuccessfulModifier, SuccessfulTransaction }
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{ InvNetworkMessage, RequestModifiersNetworkMessage }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import encry.mpg.MemoryPool._

import scala.collection.IndexedSeq

class MemoryPool(
  settings: EncryAppSettings,
  networkTimeProvider: NetworkTimeProvider,
  influxReference: Option[ActorRef],
  mempoolProcessor: ActorRef
) extends Actor
    with StrictLogging {

  import context.dispatcher

  var memoryPool: MemoryPoolStorage = MemoryPoolStorage.empty(settings, networkTimeProvider)

  var bloomFilterForTransactionsIds: BloomFilter[String] = initBloomFilter

  var canProcessTransactions: Boolean = false

  var chainSynced: Boolean = false

  override def preStart(): Unit = {
    logger.debug(s"Starting MemoryPool. Initializing all schedulers")
    context.system.scheduler.schedule(
      settings.mempool.cleanupInterval,
      settings.mempool.cleanupInterval,
      self,
      RemoveExpiredFromPool
    )
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
  }

  override def receive: Receive = continueProcessing(currentNumberOfProcessedTransactions = 0)

  def continueProcessing(currentNumberOfProcessedTransactions: Int): Receive =
    transactionsProcessor(currentNumberOfProcessedTransactions)
      .orElse(auxiliaryReceive(MemoryPoolStateType.ProcessingNewTransaction))

  def disableTransactionsProcessor: Receive = auxiliaryReceive(MemoryPoolStateType.NotProcessingNewTransactions)

  def transactionsProcessor(currentNumberOfProcessedTransactions: Int): Receive = {
    case DataFromPeer(message, remote) =>
      message match {
        case RequestModifiersNetworkMessage((_, requestedIds)) =>
          val modifiersIds: Seq[Transaction] = requestedIds
            .map(Algos.encode)
            .collect { case id if memoryPool.contains(id) => memoryPool.get(id) }
            .flatten
          logger.debug(
            s"MemoryPool got request modifiers message. Number of requested ids is ${requestedIds.size}." +
              s" Number of sent transactions is ${modifiersIds.size}. Request was from $remote."
          )
          context.parent ! ResponseFromLocal(
            remote,
            Transaction.modifierTypeId,
            modifiersIds.map(tx => tx.id -> tx.bytes).toMap
          )
        case InvNetworkMessage((_, txs)) =>
          val notYetRequestedTransactions: IndexedSeq[ModifierId] = notRequestedYet(txs.toIndexedSeq)
          if (notYetRequestedTransactions.nonEmpty) {
            sender ! RequestFromLocal(Some(remote), Transaction.modifierTypeId, notYetRequestedTransactions.toList)
            logger.debug(
              s"MemoryPool got inv message with ${txs.size} ids." +
                s" Not yet requested ids size is ${notYetRequestedTransactions.size}."
            )
          } else
            logger.debug(
              s"MemoryPool got inv message with ${txs.size} ids." +
                s" There are no not yet requested ids."
            )

        case InvNetworkMessage(invData) =>
          logger.debug(
            s"Get inv with tx: ${invData._2.map(Algos.encode).mkString(",")}, but " +
              s"chainSynced is $chainSynced and canProcessTransactions is $canProcessTransactions."
          )

        case _ => logger.debug(s"MemoryPoolProcessor got invalid type of DataFromPeer message!")
      }

    case NewTransaction(transaction) =>
      val (newMemoryPool: MemoryPoolStorage, validatedTransaction: Option[Transaction]) =
        memoryPool.validateTransaction(transaction)
      memoryPool = newMemoryPool
      mempoolProcessor ! UpdateMempoolReader(MemoryPoolReader.apply(memoryPool))
      validatedTransaction.foreach(tx => context.system.eventStream.publish(SuccessfulTransaction(tx)))
      logger.debug(s"MemoryPool got new transactions from remote. New pool size is ${memoryPool.size}.")
      if (currentNumberOfProcessedTransactions > settings.mempool.transactionsLimit) {
        logger.debug(
          s"MemoryPool has its limit of processed transactions. " +
            s"Transit to 'disableTransactionsProcessor' state." +
            s"Current number of processed transactions is $currentNumberOfProcessedTransactions."
        )
        canProcessTransactions = false
        context.parent ! TransactionProcessing(canProcessTransactions)
        context.become(disableTransactionsProcessor)
      } else {
        val currentTransactionsNumber: Int = currentNumberOfProcessedTransactions + 1
        logger.debug(
          s"Current number of processed transactions is OK. Continue to process them..." +
            s" Current number is $currentTransactionsNumber."
        )
        context.become(continueProcessing(currentTransactionsNumber))
      }

    case RolledBackTransactions(transactions) =>
      val (newMemoryPool: MemoryPoolStorage, validatedTransactions: Seq[Transaction]) =
        memoryPool.validateTransactions(transactions)
      memoryPool = newMemoryPool
      mempoolProcessor ! UpdateMempoolReader(MemoryPoolReader.apply(memoryPool))
      logger.debug(
        s"MemoryPool got rolled back transactions. New pool size is ${memoryPool.size}." +
          s"Number of rolled back transactions is ${validatedTransactions.size}."
      )
      if (currentNumberOfProcessedTransactions > settings.mempool.transactionsLimit) {
        logger.debug(
          s"MemoryPool has its limit of processed transactions. " +
            s"Transit to 'disableTransactionsProcessor' state." +
            s"Current number of processed transactions is $currentNumberOfProcessedTransactions."
        )
        canProcessTransactions = false
        context.parent ! TransactionProcessing(canProcessTransactions)
        context.become(disableTransactionsProcessor)
      } else {
        val currentTransactionsNumber: Int = currentNumberOfProcessedTransactions + validatedTransactions.size
        logger.debug(
          s"Current number of processed transactions is OK. Continue to process them..." +
            s" Current number is $currentTransactionsNumber."
        )
        context.become(continueProcessing(currentTransactionsNumber))
      }
  }

  def auxiliaryReceive(state: MemoryPoolStateType): Receive = {
    case SemanticallySuccessfulModifier(modifier) if modifier.modifierTypeId == Block.modifierTypeId =>
      logger.debug(
        s"MemoryPool got SemanticallySuccessfulModifier with new block while $state." +
          s"Transit to a transactionsProcessor state."
      )
      if (state == NotProcessingNewTransactions)
        Either.catchNonFatal(context.system.actorSelection("/user/nodeViewSynchronizer") ! StartTransactionsValidation)
      canProcessTransactions = true
      context.parent ! TransactionProcessing(canProcessTransactions)
      context.become(continueProcessing(currentNumberOfProcessedTransactions = 0))

    case SemanticallySuccessfulModifier(_) =>
      logger.debug(
        s"MemoryPool got SemanticallySuccessfulModifier with non block modifier" +
          s"while $state. Do nothing in this case."
      )

    case SendTransactionsToMiner =>
      val (newMemoryPool: MemoryPoolStorage, transactionsForMiner: Seq[Transaction]) =
        memoryPool.getTransactionsForMiner
      memoryPool = newMemoryPool
      mempoolProcessor ! UpdateMempoolReader(MemoryPoolReader.apply(memoryPool))
      sender() ! TransactionsForMiner(transactionsForMiner)
      logger.debug(
        s"MemoryPool got SendTransactionsToMiner. Size of transactions for miner ${transactionsForMiner.size}." +
          s" New pool size is ${memoryPool.size}. Ids ${transactionsForMiner.map(_.encodedId)}"
      )

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

  def notRequestedYet(ids: IndexedSeq[ModifierId]): IndexedSeq[ModifierId] = ids.collect {
    case id: ModifierId if !bloomFilterForTransactionsIds.mightContain(Algos.encode(id)) =>
      bloomFilterForTransactionsIds.put(Algos.encode(id))
      id
  }

  def initBloomFilter: BloomFilter[String] = BloomFilter.create(
    Funnels.stringFunnel(Charsets.UTF_8),
    settings.mempool.bloomFilterCapacity,
    settings.mempool.bloomFilterFailureProbability
  )
}

object MemoryPool {

  final case class NewTransaction(tx: Transaction) extends AnyVal

  final case class RolledBackTransactions(txs: IndexedSeq[Transaction])

  final case class TransactionsForMiner(txs: Seq[Transaction])

  case object SendTransactionsToMiner

  case class TransactionProcessing(info: Boolean)

  case object RemoveExpiredFromPool

  case object StopTransactionsValidation

  case object StartTransactionsValidation

  sealed trait MemoryPoolStateType

  final case class UpdateMempoolReader(reader: MemoryPoolReader)

  object MemoryPoolStateType {

    case object ProcessingNewTransaction extends MemoryPoolStateType

    case object NotProcessingNewTransactions extends MemoryPoolStateType

  }

  def props(
    settings: EncryAppSettings,
    ntp: NetworkTimeProvider,
    influx: Option[ActorRef],
    mempoolProcessor: ActorRef
  ): Props = Props(new MemoryPool(settings, ntp, influx, mempoolProcessor))

  class MemoryPoolPriorityQueue(settings: ActorSystem.Settings, config: Config)
      extends UnboundedStablePriorityMailbox(PriorityGenerator {
        case RemoveExpiredFromPool | SendTransactionsToMiner => 0
        case NewTransaction(_)                               => 1
        case otherwise                                       => 2
      })

}