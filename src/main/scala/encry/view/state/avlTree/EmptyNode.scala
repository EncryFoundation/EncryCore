package encry.view.state.avlTree

import cats.kernel.Monoid

final case class EmptyNode[K, V] private (key: K, value: V, height: Int, balance: Int) extends Node[K, V]

object EmptyNode {
  def apply[K, V]()(implicit k: Monoid[K],
                    v: Monoid[V]): EmptyNode[K, V] = new EmptyNode(k.empty, v.empty, height = 0, balance = 0)
}
