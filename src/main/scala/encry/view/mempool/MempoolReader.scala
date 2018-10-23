package encry.view.mempool

import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.Transaction
import encry.view.NodeViewComponent
import encry.view.mempool.Mempool.{MemPoolRequest, MemPoolResponse, TxKey}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Success

trait MempoolReader extends NodeViewComponent {

  protected[mempool] var waitedForAssembly: Map[Set[TxKey], (Promise[MemPoolResponse], Seq[ModifierId])] = Map.empty

  val unconfirmed: TrieMap[TxKey, Transaction]

  protected def key(id: ModifierId): TxKey = new mutable.WrappedArray.ofByte(id)

  def getById(id: ModifierId): Option[Transaction] = unconfirmed.get(key(id))

  def getAll(ids: Seq[ModifierId]): Seq[Transaction] = ids.flatMap(getById)

  def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  def size: Int = unconfirmed.size

  protected def completeAssembly(txs: Iterable[Transaction]): Unit = synchronized {
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

}
