package encry.view.state.avlTree.utils.implicits

import org.encryfoundation.common.utils.Algos

trait Hashable[A] {
  def hash(value: A): Array[Byte]
}