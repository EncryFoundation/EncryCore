package encry.view.mempool

import encry.ModifierId
import encry.view.NodeViewComponent
import encry.modifiers.mempool.Transaction

/**
  * Unconfirmed transactions pool
  *
  * @tparam TX -type of transaction the pool contains
  */
trait MempoolReader[TX <: Transaction] extends NodeViewComponent {

  //getters
  def getById(id: ModifierId): Option[TX]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[TX]

  def size: Int

  def take(limit: Int): Iterable[TX]

}