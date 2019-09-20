package encry.view.state.avlTree

import encry.view.state.avlTree.AvlTree.InsertDirection

trait Node[K, V] {

  val key: K
  val value: V
  val height: Int
  val balance: Int
  val insertDirection: InsertDirection
  def selfInspection: Node[K, V]
}
