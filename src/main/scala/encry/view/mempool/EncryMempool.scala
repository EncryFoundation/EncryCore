package encry.view.mempool

import encry.ModifierId
import encry.modifiers.mempool.Transaction
import encry.settings.EncryAppSettings
import encry.utils.{Logging, NetworkTimeProvider}
import encry.view.mempool.EncryMempool._
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

class EncryMempool(val unconfirmed: TrieMap[TxKey, Transaction],
                   settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
  extends MemoryPool[Transaction, EncryMempool] with EncryMempoolReader with AutoCloseable with Logging {

  private implicit val cleanupScheduler: Scheduler = Scheduler.singleThread("mempool-cleanup-thread")

  private val removeExpired: Task[EncryMempool] = Task {
    filter(tx => (timeProvider.time() - tx.timestamp) > settings.node.utxMaxAge.toMillis)
  }.delayExecution(settings.node.mempoolCleanupInterval)

  private val cleanup: CancelableFuture[EncryMempool] = removeExpired.runAsync

  override def close(): Unit = cleanup.cancel()

  override def put(tx: Transaction): Try[EncryMempool] = put(Seq(tx))

  override def put(txs: Iterable[Transaction]): Try[EncryMempool] = {
    val validTxs: Iterable[Transaction] = txs.filter(tx => tx.semanticValidity.isSuccess && !unconfirmed.contains(key(tx.id)))
    if (validTxs.nonEmpty) {
      if ((size + validTxs.size) <= settings.node.mempoolMaxCapacity) {
        Success(putWithoutCheck(validTxs))
      } else {
        val overflow: Int = (size + validTxs.size) - settings.node.mempoolMaxCapacity
        Success(putWithoutCheck(validTxs.take(validTxs.size - overflow)))
      }
    } else Failure(new Exception("Failed to put transaction into pool"))
  }

  override def putWithoutCheck(txs: Iterable[Transaction]): EncryMempool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    completeAssembly(txs)
    this
  }

  override def remove(tx: Transaction): EncryMempool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  def removeAsync(txs: Seq[Transaction]): Unit = Task {
    txs.foreach(remove)
  }.runAsync

  override def take(limit: Int): Iterable[Transaction] = unconfirmed.values.toSeq.take(limit)

  def takeAll: Iterable[Transaction] = unconfirmed.values.toSeq

  override def filter(condition: (Transaction) => Boolean): EncryMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }
}

object EncryMempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[Transaction]

  def empty(settings: EncryAppSettings, timeProvider: NetworkTimeProvider): EncryMempool =
    new EncryMempool(TrieMap.empty, settings, timeProvider)
}
