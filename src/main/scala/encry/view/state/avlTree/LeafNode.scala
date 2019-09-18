package encry.view.state.avlTree

final case class LeafNode[K, V](key: K,
                                value: V) extends Node[K, V] {

  override val balance: Int = 0

  override val height: Int = 0

  override def toString: String = s"($key, $value, height: 0, balance: 0)"
}
