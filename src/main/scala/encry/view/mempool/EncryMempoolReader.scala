package encry.view.mempool

import encry.ModifierId
import encry.modifiers.mempool.BaseTransaction
import encry.view.mempool.EncryMempool.{MemPoolRequest, MemPoolResponse, TxKey}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Success

trait EncryMempoolReader extends MempoolReader[BaseTransaction] {

  /**
    * Map stores current state of waiting for query building
    * value - promise of result and set of all transactions of request
    * key - set of transactions that are waiting for the assembly
    */
  protected[mempool] var waitedForAssembly: Map[Set[TxKey], (Promise[MemPoolResponse], Seq[ModifierId])] = Map.empty

  val unconfirmed: TrieMap[TxKey, BaseTransaction]

  protected def key(id: ModifierId): TxKey = new mutable.WrappedArray.ofByte(id)

  override def getById(id: ModifierId): Option[BaseTransaction] = unconfirmed.get(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[BaseTransaction] = ids.flatMap(getById)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def size: Int = unconfirmed.size

  protected def completeAssembly(txs: Iterable[BaseTransaction]): Unit = synchronized {
    val txsIds: Iterable[TxKey] = txs.map(tx => key(tx.id))
    val newMap: Map[Set[TxKey], (Promise[MemPoolResponse], Seq[ModifierId])] = waitedForAssembly.flatMap(p => {
      val ids: Set[TxKey] = p._1
      val newKey: Set[TxKey] = ids -- txsIds
      // filtering fully-built queries and completing of a promise
      if (newKey.isEmpty) {
        val (promise: Promise[MemPoolResponse], allIds: Seq[ModifierId]) = p._2
        promise.complete(Success(allIds.map(id => getById(id).get)))
        None
      } else {
        Some(newKey -> p._2)
      }
    })
    waitedForAssembly = newMap
  }

  def waitForAll(ids: MemPoolRequest): Future[MemPoolResponse] = synchronized {
    val promise: Promise[Seq[BaseTransaction]] = Promise[Seq[BaseTransaction]]
    waitedForAssembly = waitedForAssembly.updated(ids.map(id => key(id)).toSet, (promise, ids))
    promise.future
  }

  def isEmpty: Boolean = size == 0
}
