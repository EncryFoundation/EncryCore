package encry.view

import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorSystem, Props}
import com.google.common.base.Charsets
import com.google.common.hash.{BloomFilter, Funnels}
import encry.modifiers.mempool.{Transaction, TransactionProtoSerializer}
import encry.network.NodeViewSynchronizer.ReceivableMessages.RequestFromLocal
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.NetworkTimeProvider
import encry.view.EncryNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import encry.view.MemoryPool.{CompareTransactionsWithUnconfirmed, TickForCleanupBloomFilter, TickForRemoveExpired, UpdatedState}
import encry.view.state.UtxoState
import org.encryfoundation.common.Algos

import scala.concurrent.duration._
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success, Try}

class MemoryPool(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor {

  type TransactionIdAsKey = scala.collection.mutable.WrappedArray.ofByte

  implicit val ec: ExecutionContextExecutor = context.dispatcher
  implicit val system: ActorSystem = context.system

  var memoryPool: HashMap[TransactionIdAsKey, Transaction] = HashMap.empty[TransactionIdAsKey, Transaction]

  var bloomFilterForMemoryPool: BloomFilter[String] = createBloomFilter

  //var state: Option[UtxoState] = Option.empty

  context.system.scheduler.schedule(1.seconds, settings.node.bloomFilterCleanupInterval, self, TickForCleanupBloomFilter)
  context.system.scheduler.schedule(1.seconds, settings.node.mempoolCleanupInterval, self, TickForRemoveExpired)

  override def receive: Receive = {
    case ModifiersFromRemote(_, filteredModifiers) =>
      val parsedModifiers: IndexedSeq[Transaction] = filteredModifiers.foldLeft(IndexedSeq.empty[Transaction]) {
        case (transactions, bytes) =>
          TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes)) match {
            case Success(value) if state.get.validate(value) => transactions :+ value
            case _ => transactions
          }
      }
      val validatedTransactions: IndexedSeq[Transaction] = parsedModifiers.filter(tx =>
        tx.semanticValidity.isSuccess && !memoryPool.contains(toKey(tx.id)))
      if (memoryPool.size + validatedTransactions.size <= settings.node.mempoolMaxCapacity) {
        val updatedMemoryPool: HashMap[TransactionIdAsKey, Transaction] = validatedTransactions.foldLeft(memoryPool) {
          case (pool, tx) => pool.updated(toKey(tx.id), tx)
        }
        memoryPool = updatedMemoryPool
      }
      else {
        val filteredMemoryPool: HashMap[TransactionIdAsKey, Transaction] = cleanMemoryPoolFromExpired(memoryPool)
        val availableNumberOfTransactions: Int = settings.node.mempoolMaxCapacity - filteredMemoryPool.size
        val transactionsForAdding: IndexedSeq[Transaction] = validatedTransactions.take(availableNumberOfTransactions)
        val updatedMemoryPool: HashMap[TransactionIdAsKey, Transaction] = transactionsForAdding.foldLeft(memoryPool) {
          case (pool, tx) => pool.updated(toKey(tx.id), tx)
        }
        memoryPool = updatedMemoryPool
      }
    case UpdatedState(stateN) => state = Some(stateN)
    case TickForRemoveExpired => memoryPool = cleanMemoryPoolFromExpired(memoryPool)
    case TickForCleanupBloomFilter => bloomFilterForMemoryPool = createBloomFilter
    case CompareTransactionsWithUnconfirmed(peer, transactions) =>
      val unrequestedModifiers: IndexedSeq[ModifierId] = notRequested(transactions)
      if (unrequestedModifiers.nonEmpty) sender ! RequestFromLocal(peer, Transaction.ModifierTypeId, unrequestedModifiers)
    case _ =>
  }

  def mainLogic(state: UtxoState): Receive = {
    case ModifiersFromRemote(_, filteredModifiers) =>
      val parsedModifiers: IndexedSeq[Transaction] = filteredModifiers.foldLeft(IndexedSeq.empty[Transaction]) {
        case (transactions, bytes) =>
          TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes)) match {
            case Success(value) => transactions :+ value
            case _ => transactions
          }
      }
      memoryPool = validateAndPutTransactions(parsedModifiers, memoryPool, state)
    case _ =>
  }

  def validateAndPutTransactions(inputTransactions: IndexedSeq[Transaction],
                                 currentMemoryPool: HashMap[TransactionIdAsKey, Transaction],
                                 currentState: UtxoState): HashMap[TransactionIdAsKey, Transaction] = {
    val validatedTransactions: IndexedSeq[Transaction] = inputTransactions.filter(tx =>
      tx.semanticValidity.isSuccess && !currentMemoryPool.contains(toKey(tx.id)) && currentState.validate(tx).isSuccess)
    if (memoryPool.size + validatedTransactions.size <= settings.node.mempoolMaxCapacity)
      validatedTransactions.foldLeft(memoryPool) { case (pool, tx) => pool.updated(toKey(tx.id), tx) }
    else {
      val filteredMemoryPool: HashMap[TransactionIdAsKey, Transaction] = cleanMemoryPoolFromExpired(memoryPool)
      val availableNumberOfTransactions: Int = settings.node.mempoolMaxCapacity - filteredMemoryPool.size
      val transactionsForAdding: IndexedSeq[Transaction] = validatedTransactions.take(availableNumberOfTransactions)
      transactionsForAdding.foldLeft(memoryPool) { case (pool, tx) => pool.updated(toKey(tx.id), tx) }
    }
  }

  def cleanMemoryPoolFromExpired(pool: HashMap[TransactionIdAsKey, Transaction]): HashMap[TransactionIdAsKey, Transaction] =
    pool.filter { case (_, tx) => (ntp.estimatedTime - tx.timestamp) > settings.node.utxMaxAge.toMillis }

  def createBloomFilter: BloomFilter[String] = BloomFilter.create(
    Funnels.stringFunnel(Charsets.UTF_8), settings.node.bloomFilterCapacity, settings.node.bloomFilterFailureProbability)

  def notRequested(ids: IndexedSeq[ModifierId]): IndexedSeq[ModifierId] = ids.collect {
    case id: ModifierId if !bloomFilterForMemoryPool.mightContain(Algos.encode(id)) =>
      bloomFilterForMemoryPool.put(Algos.encode(id))
      id
  }

  def toKey(id: ModifierId): TransactionIdAsKey = new mutable.WrappedArray.ofByte(id)

}

object MemoryPool {

  case class CompareTransactionsWithUnconfirmed(peer: ConnectedPeer, transactions: IndexedSeq[ModifierId])

  case class TransactionsFromRollBack()

  case class UpdatedState(state: UtxoState)

  case object TickForRemoveExpired

  case object TickForCleanupBloomFilter

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider): Props = Props(new MemoryPool(settings, ntp))
}