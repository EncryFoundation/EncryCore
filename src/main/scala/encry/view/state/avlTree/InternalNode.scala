package encry.view.state.avlTree

final case class InternalNode[K, V](key: K,
                                    value: V,
                                    leftChild: Option[Node[K, V]],
                                    rightChild: Option[Node[K, V]]) extends Node[K, V] {
  override def toString: String = s"[($key, $value) -> LeftChild:${leftChild.map(_.toString)}, RightChild: ${rightChild.map(_.toString)}]"
}

object InternalNode {
  def apply[K, V](key: K, value: V): InternalNode[K, V] = new InternalNode(key, value, None, None)
}
