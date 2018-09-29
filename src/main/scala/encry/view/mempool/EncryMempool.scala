package encry.view.mempool

import akka.actor.Cancellable
import akka.actor.{ActorSystem, Cancellable}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.Transaction
import encry.settings.EncryAppSettings
import encry.utils.{Logging, NetworkTimeProvider}
import encry.view.mempool.EncryMempool._
import org.encryfoundation.common.Algos

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

class EncryMempool(val unconfirmed: TrieMap[TxKey, Transaction],
                   settings: EncryAppSettings, timeProvider: NetworkTimeProvider, system: ActorSystem)
  extends MemoryPool[Transaction, EncryMempool] with EncryMempoolReader with AutoCloseable with Logging {

  private def removeExpired(): EncryMempool =
    filter(tx => (timeProvider.estimatedTime - tx.timestamp) > settings.node.utxMaxAge.toMillis)

  private val cleanup: Cancellable =
    system.scheduler.schedule(settings.node.mempoolCleanupInterval, settings.node.mempoolCleanupInterval)(removeExpired)

  override def close(): Unit = cleanup.cancel()

  override def put(tx: Transaction): Try[EncryMempool] = put(Seq(tx))

  override def put(txs: Iterable[Transaction]): Try[EncryMempool] = {
    val validTxs: Iterable[Transaction] = txs
      .filter{tx =>
        logWarn(s"${Algos.encode(tx.id)} semanticValid - ${tx.semanticValidity.isSuccess}  unconf - ${!unconfirmed.contains(key(tx.id))}")
        println(s"${Algos.encode(tx.id)} semantic valid - ${tx.semanticValidity.isSuccess}  unconf - ${!unconfirmed.contains(key(tx.id))}")
        println(s"${tx.semanticValidity match {
          case Failure(exception) => exception
          case _ => "all is good"
        }}")
        tx.semanticValidity.isSuccess && !unconfirmed.contains(key(tx.id))}
    if (validTxs.nonEmpty) {
      if ((size + validTxs.size) <= settings.node.mempoolMaxCapacity) {
        Success(putWithoutCheck(validTxs))
      } else {
        removeExpired()
        val overflow: Int = (size + validTxs.size) - settings.node.mempoolMaxCapacity
        Success(putWithoutCheck(validTxs.take(validTxs.size - overflow)))
      }
    } else {
      logWarn("Failed to put transaction into pool cause all invalid")
      Failure(new Exception("Failed to put transaction into pool"))}
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

  def removeAsync(txs: Seq[Transaction]): Unit = Future {
    txs.foreach(remove)
  }

  override def take(limit: Int): Iterable[Transaction] = unconfirmed.values.toSeq.take(limit)

  def takeAll: Iterable[Transaction] = unconfirmed.values.toSeq

  override def filter(condition: Transaction => Boolean): EncryMempool = {
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

  def empty(settings: EncryAppSettings, timeProvider: NetworkTimeProvider, system: ActorSystem): EncryMempool =
    new EncryMempool(TrieMap.empty, settings, timeProvider, system)
}
