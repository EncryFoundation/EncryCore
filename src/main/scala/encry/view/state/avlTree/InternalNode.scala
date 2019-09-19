package encry.view.state.avlTree

import encry.view.state.avlTree.AvlTree.InsertDirection

final case class InternalNode[K, V](key: K,
                                    value: V,
                                    height: Int,
                                    balance: Int,
                                    leftChild: Option[Node[K, V]],
                                    rightChild: Option[Node[K, V]],
                                    insertDirection: InsertDirection) extends Node[K, V] {

  override def selfInspection: Node[K, V] = if (leftChild.isEmpty & rightChild.isEmpty) LeafNode(key, value)
                                            else this

  override def toString: String = s"[($key, $value, height: $height, balance $balance, $insertDirection) \n-> LeftChildOf($key):${leftChild.map(_.toString)}, \n-> RightChildOf($key): ${rightChild.map(_.toString)}]"
}

object InternalNode {
  def apply[K, V](key: K,
                  value: V,
                  height: Int,
                  balance: Int,
                  insertDirection: InsertDirection): InternalNode[K, V] = new InternalNode(key, value, height, balance, None, None, insertDirection)
}
