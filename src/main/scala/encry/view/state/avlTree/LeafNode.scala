package encry.view.state.avlTree

final case class LeafNode[K, V](key: K,
                                value: V) extends Node[K, V] {

  override def toString: String = s"($key, $value)"
}
