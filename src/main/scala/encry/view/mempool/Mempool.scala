package encry.view.mempool

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.google.common.base.Charsets
import com.google.common.hash.{BloomFilter, Funnels}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages.SuccessfulTransaction
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.MempoolStat
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import encry.view.mempool.Mempool._
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId, ModifierTypeId}
import scala.collection.immutable.HashMap
import scala.collection.{IndexedSeq, mutable}
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

class Mempool(settings: EncryAppSettings,
              ntp: NetworkTimeProvider,
              minerRef: ActorRef,
              influx: Option[ActorRef]) extends Actor with StrictLogging {

  type WrappedIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  implicit val system: ActorSystem = context.system

  var memoryPool: HashMap[WrappedIdAsKey, Transaction] = HashMap.empty[WrappedIdAsKey, Transaction]

  var bloomFilterForTransactionsIds: BloomFilter[String] = initBloomFilter

  var bloomFilterForBoxesIds: BloomFilter[String] = initBloomFilter

  override def receive: Receive = {
    case _@UpdatedState(updatedState) =>
      updatedState match {
        case utxoState: UtxoState => logger.debug(s"Received state instance on MemoryPool actor. Starting mainLogic on this actor.")
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
            5.seconds)(influx.foreach(_ ! MempoolStat(memoryPool.size)))
          context.become(messagesHandler(utxoState))
        //case digestState: DigestState => logger.info(s"Got digest state on MemoryPool actor.")
      }
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
    case CompareTransactionsWithUnconfirmed(peer, transactions) =>
      val unrequestedModifiers: IndexedSeq[ModifierId] = notRequested(transactions)
      if (unrequestedModifiers.nonEmpty) sender ! RequestForTransactions(peer, Transaction.modifierTypeId, unrequestedModifiers)
    case RolledBackTransactions(txs) => memoryPool = validateAndPutTransactions(txs, memoryPool, state, fromNetwork = false)
    case TransactionsForRemove(txs) =>
      memoryPool = removeOldTransactions(txs, memoryPool)
    case LocallyGeneratedTransaction(tx) =>
      memoryPool = validateAndPutTransactions(IndexedSeq(tx), memoryPool, state, fromNetwork = true)
    case TickForSendTransactionsToMiner =>
      val validatedTxs: (IndexedSeq[WrappedIdAsKey], IndexedSeq[Transaction]) =
        memoryPool.values.toIndexedSeq.sortBy(_.fee).reverse
          .foldLeft(IndexedSeq.empty[WrappedIdAsKey], IndexedSeq.empty[Transaction]) { case ((boxes, txs), tx) =>
            val txInputsIds: Set[WrappedIdAsKey] = tx.inputs.map(input => toKey(ModifierId @@ input.boxId.untag(ADKey))).toSet
            if (txInputsIds.forall(id => !boxes.contains(id)) && txInputsIds.size == tx.inputs.size)
              (boxes ++: txInputsIds.toIndexedSeq, txs :+ tx)
            else (boxes, txs)
          }
      memoryPool = memoryPool -- validatedTxs._2.map(tx => toKey(tx.id))
      minerRef ! TxsForMiner(validatedTxs._2)
    case AskTransactionsFromNVS(remote, ids) =>
      val idsToWrapped: Seq[WrappedIdAsKey] = ids.map(toKey)
      val txsForNVS: Seq[Transaction] = idsToWrapped.flatMap(id => memoryPool.get(id))
      sender() ! TxsForNVSH(remote, txsForNVS)
  }

  def handleStates: Receive = {
    case _@UpdatedState(updatedState) =>
      updatedState match {
        case utxoState: UtxoState => context.become(messagesHandler(utxoState))
        //case digestState: DigestState => logger.info(s"Got digest state on MemoryPool actor.")
      }
    case msg => logger.info(s"Got strange message on MemoryPool actor $msg.")
  }

  def removeOldTransactions(txs: IndexedSeq[Transaction],
                            pool: HashMap[WrappedIdAsKey, Transaction]): HashMap[WrappedIdAsKey, Transaction] = {
    val transactionsIds: IndexedSeq[WrappedIdAsKey] = txs.map(tx => toKey(tx.id))
    pool -- transactionsIds
  }

  def validateAndPutTransactions(inputTransactions: IndexedSeq[Transaction],
                                 currentMemoryPool: HashMap[WrappedIdAsKey, Transaction],
                                 currentState: UtxoState,
                                 fromNetwork: Boolean): HashMap[WrappedIdAsKey, Transaction] = {
    val validatedTransactions: IndexedSeq[Transaction] = inputTransactions.filter(tx =>
      tx.semanticValidity.isSuccess && !currentMemoryPool.contains(toKey(tx.id))
      //&& currentState.validate(tx).isSuccess
    )
    if (memoryPool.size + validatedTransactions.size <= settings.node.mempoolMaxCapacity)
      validatedTransactions.foldLeft(memoryPool) { case (pool, tx) =>
        if (fromNetwork) context.system.eventStream.publish(SuccessfulTransaction(tx))
        pool.updated(toKey(tx.id), tx)
      }
    else {
      val filteredMemoryPool: HashMap[WrappedIdAsKey, Transaction] = cleanMemoryPoolFromExpired(memoryPool)
      val availableNumberOfTransactions: Int = settings.node.mempoolMaxCapacity - filteredMemoryPool.size
      val transactionsForAdding: IndexedSeq[Transaction] = validatedTransactions.take(availableNumberOfTransactions)
      transactionsForAdding.foldLeft(memoryPool) { case (pool, tx) =>
        if (fromNetwork) context.system.eventStream.publish(SuccessfulTransaction(tx))
        pool.updated(toKey(tx.id), tx)
      }
    }
  }

  def cleanMemoryPoolFromExpired(pool: HashMap[WrappedIdAsKey, Transaction]): HashMap[WrappedIdAsKey, Transaction] =
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

  def toKey(id: ModifierId): WrappedIdAsKey = new mutable.WrappedArray.ofByte(id)

}

object Mempool {

  final case class TransactionsFromRemote(tx: Seq[Transaction])

  case class CompareTransactionsWithUnconfirmed(peer: ConnectedPeer, transactions: IndexedSeq[ModifierId])

  case class TransactionsFromMemoryPool(txs: IndexedSeq[Transaction])

  case class TransactionsForRemove(txs: IndexedSeq[Transaction])

  case class RolledBackTransactions(txs: IndexedSeq[Transaction])

  case class UpdatedState(state: UtxoState)

  case object TickForSendTransactionsToMiner

  case object TickForRemoveExpired

  case object TickForCleanupBloomFilter

  case object GetMempoolSize

  case object AskTransactionsFromMemoryPoolFromMiner

  case class AskTransactionsFromNVS(peer: ConnectedPeer, txsIds: Seq[ModifierId])

  case class TxsForMiner(txs: IndexedSeq[Transaction])

  case class TxsForNVSH(peer: ConnectedPeer, txs: Seq[Transaction])

  final case class RequestForTransactions(source: ConnectedPeer,
                                          modifierTypeId: ModifierTypeId,
                                          modifierIds: Seq[ModifierId])

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider, minerRef: ActorRef, influx: Option[ActorRef]): Props =
    Props(new Mempool(settings, ntp, minerRef, influx))

  class MempoolPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      // Create a new PriorityGenerator, lower priority means more important
      PriorityGenerator {
        // 'highpriority messages should be treated first if possible
        case UpdatedState(_) => 0

        case AskTransactionsFromMemoryPoolFromMiner | TransactionsForRemove => 1
        // We default to 1, which is in between high and low
        case otherwise => 2
      })

}