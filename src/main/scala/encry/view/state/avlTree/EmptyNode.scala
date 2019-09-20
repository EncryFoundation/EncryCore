package encry.view.state.avlTree

import cats.kernel.Monoid
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.EMPTY

final case class EmptyNode[K, V] private (key: K, value: V, height: Int, balance: Int) extends Node[K, V] {

  override def selfInspection: Node[K, V] = this

  override val hash: Array[Byte] = Array.emptyByteArray
}

object EmptyNode {
  def apply[K, V]()(implicit k: Monoid[K],
                    v: Monoid[V]): EmptyNode[K, V] = new EmptyNode(k.empty, v.empty, height = 0, balance = 0)
}
