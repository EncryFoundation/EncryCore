package encry.view.mempool

import encry.modifiers.mempool.Transaction

import scala.util.Try

trait MemoryPool[TX <: Transaction, M <: MemoryPool[TX, M]] extends MempoolReader[TX] {

  def put(tx: TX): Try[M]

  def put(txs: Iterable[TX]): Try[M]

  def putWithoutCheck(txs: Iterable[TX]): M

  def remove(tx: TX): M

  def filter(txs: Seq[TX]): M = filter(t => !txs.exists(_.id sameElements t.id))

  def filter(condition: TX => Boolean): M

  def getReader: MempoolReader[TX] = this
}