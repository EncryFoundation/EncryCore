package encry.view.mempool

import encry.modifiers.mempool.EncryBaseTransaction
import scorex.core.{ModifierId, NodeViewComponent}

import scala.util.Try

trait EncryBaseMemoryPool[M <: EncryBaseMemoryPool[M]] extends NodeViewComponent {

  def getById(id: ModifierId): Option[EncryBaseTransaction]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[EncryBaseTransaction]

  /**
    * Method to put a transaction into the memory pool. Validation of tha transactions against
    * the state is done in NodeVieHolder. This put() method can check whether a transaction is valid
    * @param tx
    * @return Success(updatedPool), if transaction successfully added to the pool, Failure(_) otherwise
    */
  def put(tx: EncryBaseTransaction): Try[M]

  def put(txs: Iterable[EncryBaseTransaction]): Try[M]

  def putWithoutCheck(txs: Iterable[EncryBaseTransaction]): M

  def remove(tx: EncryBaseTransaction): M

  def take(limit: Int): Iterable[EncryBaseTransaction]

  def filter(txs: Seq[EncryBaseTransaction]): M

  def filter(condition: EncryBaseTransaction => Boolean): M

  def size: Int

}
