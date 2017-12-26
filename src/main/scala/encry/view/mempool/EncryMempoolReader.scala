package encry.view.mempool

import encry.modifiers.mempool.EncryBaseTransaction
import scorex.core.{ModifierId, NodeViewComponent}
import scorex.core.transaction.Transaction

/**
  * Unconfirmed transactions pool
  */
trait EncryMempoolReader extends NodeViewComponent {

  //getters
  def getById(id: ModifierId): Option[EncryBaseTransaction]

  def contains(id: ModifierId): Boolean

  //get ids from Seq, not presenting in mempool
  def notIn(ids: Seq[ModifierId]): Seq[ModifierId] = ids.filter(id => !contains(id))

  def getAll(ids: Seq[ModifierId]): Seq[EncryBaseTransaction]

  def size: Int
}
