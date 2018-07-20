package encry.view.mempool

import encry.ModifierId
import encry.modifiers.mempool.BaseTransaction
import encry.settings.EncryAppSettings
import encry.utils.{Logging, NetworkTimeProvider}
import encry.view.mempool.EncryMempool._
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

class EncryMempool(val unconfirmed: TrieMap[TxKey, BaseTransaction],
                   settings: EncryAppSettings, timeProvider: NetworkTimeProvider)
  extends MemoryPool[BaseTransaction, EncryMempool] with EncryMempoolReader with AutoCloseable with Logging {

  private implicit val cleanupScheduler: Scheduler = Scheduler.singleThread("mempool-cleanup-thread")

  private val removeExpired: Task[EncryMempool] = Task {
    filter(tx => (timeProvider.time() - tx.timestamp) > settings.node.utxMaxAge.toMillis)
  }.delayExecution(settings.node.mempoolCleanupInterval)

  private val cleanup: CancelableFuture[EncryMempool] = removeExpired.runAsync

  override def close(): Unit = cleanup.cancel()

  override def put(tx: BaseTransaction): Try[EncryMempool] = put(Seq(tx))

  override def put(txs: Iterable[BaseTransaction]): Try[EncryMempool] = {
    val validTxs: Iterable[BaseTransaction] = txs.filter(tx => tx.semanticValidity.isSuccess && !unconfirmed.contains(key(tx.id)))
    if (validTxs.nonEmpty) {
      if ((size + validTxs.size) <= settings.node.mempoolMaxCapacity) {
        Success(putWithoutCheck(validTxs))
      } else {
        val overflow: Int = (size + validTxs.size) - settings.node.mempoolMaxCapacity
        Success(putWithoutCheck(validTxs.take(validTxs.size - overflow)))
      }
    } else Failure(new Exception("Failed to put transaction into pool"))
  }

  override def putWithoutCheck(txs: Iterable[BaseTransaction]): EncryMempool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    completeAssembly(txs)
    this
  }

  override def remove(tx: BaseTransaction): EncryMempool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  def removeAsync(txs: Seq[BaseTransaction]): Unit = Task {
    txs.foreach(remove)
  }.runAsync

  override def take(limit: Int): Iterable[BaseTransaction] = unconfirmed.values.toSeq.take(limit)

  def takeAll: Iterable[BaseTransaction] = unconfirmed.values.toSeq

  override def filter(condition: (BaseTransaction) => Boolean): EncryMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }
}

object EncryMempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[BaseTransaction]

  def empty(settings: EncryAppSettings, timeProvider: NetworkTimeProvider): EncryMempool =
    new EncryMempool(TrieMap.empty, settings, timeProvider)
}
