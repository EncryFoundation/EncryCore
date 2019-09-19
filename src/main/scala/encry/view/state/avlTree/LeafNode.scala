package encry.view.state.avlTree

import encry.view.state.avlTree.AvlTree.InsertDirection
import encry.view.state.avlTree.AvlTree.InsertDirections.EMPTY

final case class LeafNode[K, V](key: K,
                                value: V) extends Node[K, V] {

  override val balance: Int = 0

  override val height: Int = 0

  override val insertDirection: InsertDirection = EMPTY

  override def selfInspection: Node[K, V] = this

  override def toString: String = s"($key, $value, height: 0, balance: 0)"
}
