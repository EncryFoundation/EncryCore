package encry.view.state.avlTree

final case class InternalNode[K, V](key: K,
                                    value: V,
                                    height: Int,
                                    balance: Int,
                                    leftChild: Option[Node[K, V]],
                                    rightChild: Option[Node[K, V]]) extends Node[K, V] {
  override def toString: String = s"[($key, $value, $height, $balance) -> LeftChild:${leftChild.map(_.toString)}, RightChild: ${rightChild.map(_.toString)}]"
}

object InternalNode {
  def apply[K, V](key: K,
                  value: V,
                  height: Int,
                  balance: Int): InternalNode[K, V] = new InternalNode(key, value, height, balance, None, None)
}
