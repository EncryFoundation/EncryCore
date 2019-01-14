package encry.view.mempool

import akka.actor.ActorSystem
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.Transaction
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.mempool.Mempool._
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

class Mempool(val unconfirmed: TrieMap[TxKey, Transaction],
              settings: EncryAppSettings, timeProvider: NetworkTimeProvider, system: ActorSystem)
  extends MempoolReader with AutoCloseable {

  private def removeExpired(): Mempool =
    filter(tx => (timeProvider.estimatedTime - tx.timestamp) > settings.node.utxMaxAge.toMillis)

  def close(): Unit = system.scheduler
    .schedule(settings.node.mempoolCleanupInterval, settings.node.mempoolCleanupInterval)(removeExpired()).cancel()

  def put(tx: Transaction): Try[Mempool] = put(Seq(tx))

  def put(txs: Iterable[Transaction]): Try[Mempool] = {
    val validTxs: Iterable[Transaction] = txs
      .filter(tx => tx.semanticValidity.isSuccess && !unconfirmed.contains(key(tx.id)))
    if (validTxs.nonEmpty) {
      if ((size + validTxs.size) <= settings.node.mempoolMaxCapacity) {
        Success(putWithoutCheck(validTxs))
      } else {
        removeExpired()
        val overflow: Int = (size + validTxs.size) - settings.node.mempoolMaxCapacity
        Success(putWithoutCheck(validTxs.take(validTxs.size - overflow)))
      }
    } else Failure(new Exception("Failed to put transaction into pool"))
  }

  def putWithoutCheck(txs: Iterable[Transaction]): Mempool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    completeAssembly(txs)
    this
  }

  def remove(tx: Transaction): Mempool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  def removeAsync(txs: Seq[Transaction]): Unit = Future {
    txs.foreach(remove)
  }

  def take(limit: Int): Iterable[Transaction] = unconfirmed.values.toSeq.take(limit)

  def takeAll: Iterable[Transaction] = unconfirmed.values.toSeq

  def filter(condition: Transaction => Boolean): Mempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))
}

object Mempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[Transaction]

  def empty(settings: EncryAppSettings, timeProvider: NetworkTimeProvider, system: ActorSystem): Mempool =
    new Mempool(TrieMap.empty, settings, timeProvider, system)
}