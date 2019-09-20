package encry.view.state.avlTree.utils.implicits

import org.encryfoundation.common.utils.Algos

trait Hashable[@specialized(Int, Long, Float, Double) A] {
  def hash(value: A): Array[Byte]
}