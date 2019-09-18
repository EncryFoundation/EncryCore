package encry.view.state.avlTree

trait Node[K, V] {
  val key: K
  val value: V
  val height: Int
  val balance: Int
}
