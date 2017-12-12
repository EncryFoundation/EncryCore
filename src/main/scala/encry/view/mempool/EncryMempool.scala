package encry.view.mempool

import encry.modifiers.mempool.EncryPaymentTransaction
import encry.view.mempool.EncryMempool._
import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.{Success, Try}

class EncryMempool private[mempool](val unconfirmed: mutable.ListMap[TxKey, EncryPaymentTransaction])
  extends MemoryPool[EncryPaymentTransaction, EncryMempool] {

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

  override def getById(id: ModifierId): Option[EncryPaymentTransaction] = unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[EncryPaymentTransaction] = ids.flatMap(getById)

  override def put(tx: EncryPaymentTransaction): Try[EncryMempool] = {
    put(Seq(tx))
  }

  override def put(txs: Iterable[EncryPaymentTransaction]): Try[EncryMempool] = Try {
    txs.foreach(tx => {
      //require(!unconfirmed.contains(key(ModifierId @@ tx.hashNoNonces)))
    })
    //todo check validity
    putWithoutCheck(txs)
  }

  override def putWithoutCheck(txs: Iterable[EncryPaymentTransaction]): EncryMempool = {
    txs.foreach(tx => {
      println("Add:" + tx.signatures)
      unconfirmed.put(key(ModifierId @@ tx.hashNoNonces), tx)
    })
    //completeAssembly(txs)
    //todo cleanup?
    this
  }

  override def remove(tx: EncryPaymentTransaction): EncryMempool = {
    unconfirmed.remove(key(ModifierId @@ tx.hashNoNonces))
    this
  }

  override def take(limit: Int): Iterable[EncryPaymentTransaction] =
    unconfirmed.values.toSeq.take(limit)

  override def filter(condition: (EncryPaymentTransaction) => Boolean): EncryMempool = {
    unconfirmed.retain { (_, v) =>
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size

  private def completeAssembly(txs: Iterable[EncryPaymentTransaction]): Unit = synchronized {
    val txsIds = txs.map(tx => key(ModifierId @@ tx.hashNoNonces))
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
    val promise = Promise[Seq[EncryPaymentTransaction]]
    waitedForAssembly = waitedForAssembly.updated(ids.map(id => key(id)).toSet, (promise, ids))
    promise.future
  }
}

object EncryMempool {

  type TxKey = scala.collection.mutable.WrappedArray.ofByte

  type MemPoolRequest = Seq[ModifierId]

  type MemPoolResponse = Seq[EncryPaymentTransaction]

  def empty: EncryMempool = new EncryMempool(mutable.ListMap.empty)
}