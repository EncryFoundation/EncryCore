package encry.mpg

import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

trait MemoryPoolReader {
  def get(elem: String): Option[Transaction]
}

object MemoryPoolReader {
  def apply(pool: MemoryPoolStorage): MemoryPoolReader = (elem: String) => pool.get(elem)

  def empty: MemoryPoolReader = (_: String) => None
}
