package encry.view.mempool

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.google.common.base.Charsets
import com.google.common.hash.{BloomFilter, Funnels}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SuccessfulTransaction}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.MempoolStat
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import encry.view.mempool.Mempool._
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId, ModifierTypeId}
import scala.collection.immutable.HashMap
import scala.collection.{IndexedSeq, mutable}
import scala.concurrent.duration._

class Mempool(settings: EncryAppSettings,
              ntp: NetworkTimeProvider,
              minerRef: ActorRef,
              influx: Option[ActorRef]) extends Actor with StrictLogging {

  import context.dispatcher

  type Key = scala.collection.mutable.WrappedArray.ofByte

  implicit val system: ActorSystem = context.system

  var currentNumberOfTransactions: Int = 0

  var canProcessNewTransactions: Boolean = true

  var memoryPool: HashMap[Key, Transaction] = HashMap.empty[Key, Transaction]

  var bloomFilterForTransactionsIds: BloomFilter[String] = initBloomFilter

  var bloomFilterForBoxesIds: BloomFilter[String] = initBloomFilter

  override def receive: Receive = {
    case UpdatedState(updatedState) =>
      logger.debug(s"Received state instance on MemoryPool actor. Starting mainLogic on this actor.")
      context.system.scheduler.schedule(
        settings.node.bloomFilterCleanupInterval,
        settings.node.bloomFilterCleanupInterval, self, TickForCleanupBloomFilter)
      context.system.scheduler.schedule(
        settings.node.mempoolCleanupInterval,
        settings.node.mempoolCleanupInterval, self, TickForRemoveExpired)
      context.system.scheduler.schedule(
        settings.node.mempoolTxSendingInterval,
        settings.node.mempoolTxSendingInterval, self, TickForSendTransactionsToMiner)
      context.system.scheduler.schedule(
        5.seconds,
        1.seconds)(influx.foreach(_ ! MempoolStat(memoryPool.size)))
      context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
      context.become(messagesHandler(updatedState))
    case GetMempoolSize => sender() ! memoryPool.size
    case msg => logger.info(s"Got strange message on MemoryPool actor $msg.")
  }

  def messagesHandler(state: UtxoState): Receive = mainLogic(state).orElse(handleStates)

  def mainLogic(state: UtxoState): Receive = {
    case TransactionsFromRemote(txs) =>
      memoryPool = validateAndPutTransactions(txs.toIndexedSeq, memoryPool, state, fromNetwork = true)
    case TickForRemoveExpired => memoryPool = cleanMemoryPoolFromExpired(memoryPool)
    case GetMempoolSize => sender() ! memoryPool.size
    case TickForCleanupBloomFilter =>
      bloomFilterForTransactionsIds = initBloomFilter
      bloomFilterForBoxesIds = initBloomFilter
    case CompareTransactionsWithUnconfirmed(peer, transactions) if canProcessNewTransactions =>
      val unrequestedModifiers: IndexedSeq[ModifierId] = notRequested(transactions)
      if (unrequestedModifiers.nonEmpty) sender ! RequestForTransactions(peer, Transaction.modifierTypeId, unrequestedModifiers)
    case CompareTransactionsWithUnconfirmed(peer, transactions) =>
      logger.info(s"Mempool got CompareTransactionsWithUnconfirmed but canProcessNewTransactions is: $canProcessNewTransactions")
    case RolledBackTransactions(txs) =>
      memoryPool = validateAndPutTransactions(txs, memoryPool, state, fromNetwork = false)
      logger.info(s"Mempool got RolledBackTransactions. Current mempool size is: ${memoryPool.size}")
    case TransactionsForRemove(txs) =>
      memoryPool = removeOldTransactions(txs, memoryPool)
    case LocallyGeneratedTransaction(tx) =>
      memoryPool = validateAndPutTransactions(IndexedSeq(tx), memoryPool, state, fromNetwork = true)
    case TickForSendTransactionsToMiner =>
      val validatedTxs: (IndexedSeq[Key], IndexedSeq[Transaction]) =
        memoryPool.values.toIndexedSeq.sortBy(_.fee).reverse
          .foldLeft(IndexedSeq.empty[Key], IndexedSeq.empty[Transaction]) { case ((boxes, txs), tx) =>
            val txInputsIds: Set[Key] = tx.inputs.map(input => toKey(ModifierId @@ input.boxId.untag(ADKey))).toSet
            if (txInputsIds.forall(id => !boxes.contains(id)) && txInputsIds.size == tx.inputs.size)
              (boxes ++: txInputsIds.toIndexedSeq, txs :+ tx)
            else (boxes, txs)
          }
      memoryPool = memoryPool -- validatedTxs._2.map(tx => toKey(tx.id))
      minerRef ! TxsForMiner(validatedTxs._2)
      currentNumberOfTransactions += validatedTxs._2.size
      logger.info(s"Sending to miner new transactions from mempool. Current threshold is: $currentNumberOfTransactions")
      if (currentNumberOfTransactions >= settings.node.mempoolTransactionsThreshold) {
        canProcessNewTransactions = false
        logger.info(s"Threshold has its limit. Stop processing new transactions.")
      }

    case AskTransactionsFromNVS(remote, ids) =>
      val idsToWrapped: Seq[Key] = ids.map(toKey)
      val txsForNVS: Seq[Transaction] = idsToWrapped.flatMap(id => memoryPool.get(id))
      sender() ! TxsForNVSH(remote, txsForNVS)

    case SemanticallySuccessfulModifier(modifier) if modifier.modifierTypeId == Block.modifierTypeId =>
      logger.info(s"Mempool actor got SemanticallySuccessfulModifier with new block. Reset all transactions params")
      currentNumberOfTransactions = 0
      canProcessNewTransactions = true
    case SemanticallySuccessfulModifier(modifier) =>
      logger.info(s"Mempool actor got message SemanticallySuccessfulModifier with modifier ${modifier.modifierTypeId}")
  }

  def handleStates: Receive = {
    case UpdatedState(updatedState) => context.become(messagesHandler(updatedState))
    case msg => logger.info(s"Got strange message on MemoryPool actor $msg.")
  }

  def removeOldTransactions(txs: IndexedSeq[Transaction],
                            pool: HashMap[Key, Transaction]): HashMap[Key, Transaction] = {
    val transactionsIds: IndexedSeq[Key] = txs.map(tx => toKey(tx.id))
    pool -- transactionsIds
  }

  def validateAndPutTransactions(inputTransactions: IndexedSeq[Transaction],
                                 currentMemoryPool: HashMap[Key, Transaction],
                                 currentState: UtxoState,
                                 fromNetwork: Boolean): HashMap[Key, Transaction] = {
    val validatedTransactions: IndexedSeq[Transaction] = inputTransactions.filter(tx =>
      tx.semanticValidity.isSuccess && !currentMemoryPool.contains(toKey(tx.id))
    )
    if (memoryPool.size + validatedTransactions.size <= settings.node.mempoolMaxCapacity)
      validatedTransactions.foldLeft(memoryPool) { case (pool, tx) =>
        if (fromNetwork) context.system.eventStream.publish(SuccessfulTransaction(tx))
        pool.updated(toKey(tx.id), tx)
      }
    else {
      val filteredMemoryPool: HashMap[Key, Transaction] = cleanMemoryPoolFromExpired(memoryPool)
      val availableNumberOfTransactions: Int = settings.node.mempoolMaxCapacity - filteredMemoryPool.size
      val transactionsForAdding: IndexedSeq[Transaction] = validatedTransactions.take(availableNumberOfTransactions)
      transactionsForAdding.foldLeft(memoryPool) { case (pool, tx) =>
        if (fromNetwork) context.system.eventStream.publish(SuccessfulTransaction(tx))
        pool.updated(toKey(tx.id), tx)
      }
    }
  }

  def cleanMemoryPoolFromExpired(pool: HashMap[Key, Transaction]): HashMap[Key, Transaction] =
    pool.filter { case (_, tx) => (ntp.estimatedTime - tx.timestamp) < settings.node.utxMaxAge.toMillis }

  def initBloomFilter: BloomFilter[String] = BloomFilter.create(
    Funnels.stringFunnel(Charsets.UTF_8), settings.node.bloomFilterCapacity, settings.node.bloomFilterFailureProbability)

  def notRequested(ids: IndexedSeq[ModifierId]): IndexedSeq[ModifierId] = ids.collect {
    case id: ModifierId if !bloomFilterForTransactionsIds.mightContain(Algos.encode(id)) =>
      bloomFilterForTransactionsIds.put(Algos.encode(id))
      id
  }

  //should check only after get block
  def containsValidBoxes(tx: Transaction): Boolean =
    if (tx.inputs.forall(input => !bloomFilterForBoxesIds.mightContain(Algos.encode(input.boxId)))) {
      tx.inputs.foreach(input => bloomFilterForBoxesIds.put(Algos.encode(input.boxId)))
      true
    } else false

  def toKey(id: ModifierId): Key = new mutable.WrappedArray.ofByte(id)

}

object Mempool {

  case object StopReceivingTransactions

  final case class TransactionsFromRemote(tx: Seq[Transaction]) extends AnyVal

  case class CompareTransactionsWithUnconfirmed(peer: ConnectedPeer, transactions: IndexedSeq[ModifierId])

  final case class TransactionsFromMemoryPool(txs: IndexedSeq[Transaction]) extends AnyVal

  final case class TransactionsForRemove(txs: IndexedSeq[Transaction]) extends AnyVal

  final case class RolledBackTransactions(txs: IndexedSeq[Transaction]) extends AnyVal

  final case class UpdatedState(state: UtxoState) extends AnyVal

  case object TickForSendTransactionsToMiner

  case object TickForRemoveExpired

  case object TickForCleanupBloomFilter

  case object GetMempoolSize

  case object AskTransactionsFromMemoryPoolFromMiner

  final case class AskTransactionsFromNVS(peer: ConnectedPeer, txsIds: Seq[ModifierId])

  final case class TxsForMiner(txs: IndexedSeq[Transaction]) extends AnyVal

  final case class TxsForNVSH(peer: ConnectedPeer, txs: Seq[Transaction])

  final case class RequestForTransactions(source: ConnectedPeer,
                                          modifierTypeId: ModifierTypeId,
                                          modifierIds: Seq[ModifierId])

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider, minerRef: ActorRef, influx: Option[ActorRef]): Props =
    Props(new Mempool(settings, ntp, minerRef, influx))

  class MempoolPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case UpdatedState(_)                                                => 0
        case AskTransactionsFromMemoryPoolFromMiner | TransactionsForRemove => 1
        case _                                                              => 2
      })
}