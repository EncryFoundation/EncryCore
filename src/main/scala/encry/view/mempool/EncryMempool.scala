package encry.view.mempool

import encry.modifiers.mempool.EncryBaseTransaction
import encry.view.mempool.EncryMempool._
import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

class EncryMempool private[mempool](val unconfirmed: TrieMap[TxKey, EncryBaseTransaction])
  extends MemoryPool[EncryBaseTransaction, EncryMempool] with EncryMempoolReader with ScorexLogging {

  override type NVCT = EncryMempool

  override def put(tx: EncryBaseTransaction): Try[EncryMempool] = put(Seq(tx))

  override def put(txs: Iterable[EncryBaseTransaction]): Try[EncryMempool] = {
    if (!txs.forall { tx =>
      !unconfirmed.contains(key(ModifierId @@ tx.id)) &&
        tx.semanticValidity.isSuccess
    }) Failure(new Error("Illegal transaction putting!"))
    else Success(putWithoutCheck(txs))
  }

  override def putWithoutCheck(txs: Iterable[EncryBaseTransaction]): EncryMempool = {
    txs.foreach(tx => unconfirmed.put(key(ModifierId @@ tx.id), tx))
    completeAssembly(txs)
    // TODO: Cleanup?
    this
  }

  override def remove(tx: EncryBaseTransaction): EncryMempool = {
    unconfirmed.remove(key(ModifierId @@ tx.id))
    this
  }

  override def take(limit: Int): Iterable[EncryBaseTransaction] =
    unconfirmed.values.toSeq.sortBy(tx => tx.fee).reverse.take(limit)

  override def filter(condition: (EncryBaseTransaction) => Boolean): EncryMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }
}

object EncryMempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[EncryBaseTransaction]

  def empty: EncryMempool = new EncryMempool(TrieMap.empty)
}