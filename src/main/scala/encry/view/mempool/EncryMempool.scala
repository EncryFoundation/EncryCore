package encry.view.mempool

import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.EncryAppSettings
import encry.view.mempool.EncryMempool._
import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration._
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

class EncryMempool private[mempool](val unconfirmed: TrieMap[TxKey, EncryBaseTransaction],
                                    settings: EncryAppSettings)
  extends MemoryPool[EncryBaseTransaction, EncryMempool] with EncryMempoolReader with ScorexLogging {

  // TODO: Cleanup():
  // TODO: <-- RemoveExpired()
  // TODO: <-- RemoveCheapest()

  override type NVCT = EncryMempool

  override def put(tx: EncryBaseTransaction): Try[EncryMempool] = put(Seq(tx))

  override def put(txs: Iterable[EncryBaseTransaction]): Try[EncryMempool] = {
    if (!txs.forall(tx => !unconfirmed.contains(key(tx.id)) && tx.semanticValidity.isSuccess))
      Failure(new Error("Failed to put transaction into pool"))
    else
      Success(putWithoutCheck(txs))
  }

  override def putWithoutCheck(txs: Iterable[EncryBaseTransaction]): EncryMempool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    completeAssembly(txs)
    this
  }

  override def remove(tx: EncryBaseTransaction): EncryMempool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[EncryBaseTransaction] = unconfirmed.values.toSeq.take(limit)

  def takeAllUnordered: Iterable[EncryBaseTransaction] = unconfirmed.values.toSeq

  override def filter(condition: (EncryBaseTransaction) => Boolean): EncryMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  def removeExpired(currentTime: Long): Unit =
    filter(tx => (currentTime - tx.timestamp).millis > settings.nodeSettings.utxMaxAge)
}

object EncryMempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[EncryBaseTransaction]

  def empty(settings: EncryAppSettings): EncryMempool = new EncryMempool(TrieMap.empty, settings)
}