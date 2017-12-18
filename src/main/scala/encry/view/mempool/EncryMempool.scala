package encry.view.mempool

import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.view.mempool.EncryMempool._
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

class EncryMempool private[mempool](val unconfirmed: mutable.ListMap[TxKey, EncryBaseTransaction])
  extends EncryBaseMemoryPool[EncryMempool] with ScorexLogging {

  override type NVCT = EncryMempool

  /**
    * Map stores current state of waiting for query building
    * value - promise of result and set of all transactions of request
    * key - set of transactions that are waiting for the assembly
    */
  private[mempool] var waitedForAssembly: Map[Set[TxKey], (Promise[MemPoolResponse], Seq[ModifierId])] = Map.empty

  private def key(id: ModifierId): TxKey = {
    new mutable.WrappedArray.ofByte(id)
  }

  override def getById(id: ModifierId): Option[EncryBaseTransaction] = unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[EncryBaseTransaction] = ids.flatMap(getById)

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
    unconfirmed.values.toSeq.take(limit)

  // TODO: What should this method do?
  override def filter(txs: Seq[EncryBaseTransaction]): EncryMempool = this

  override def filter(condition: (EncryBaseTransaction) => Boolean): EncryMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size

  private def completeAssembly(txs: Iterable[EncryBaseTransaction]): Unit = synchronized {
    val txsIds = txs.map(tx => key(ModifierId @@ tx.id))
    val newMap = waitedForAssembly.flatMap(p => {
      val ids = p._1
      val newKey = ids -- txsIds
      // filtering fully-built queries and completing of a promise
      if (newKey.isEmpty) {
        val (promise, allIds) = p._2
        promise complete Success(allIds.map(id => getById(id).get))
        None
      } else {
        Some(newKey -> p._2)
      }
    })
    waitedForAssembly = newMap
  }

  def waitForAll(ids: MemPoolRequest): Future[MemPoolResponse] = synchronized {
    val promise = Promise[Seq[EncryBaseTransaction]]
    waitedForAssembly = waitedForAssembly.updated(ids.map(id => key(id)).toSet, (promise, ids))
    promise.future
  }
}

object EncryMempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[EncryBaseTransaction]

  def empty: EncryMempool = new EncryMempool(mutable.ListMap.empty)
}