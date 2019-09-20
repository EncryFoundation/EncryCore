package encry.view.state.avlTree

import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.utils.implicits.Hashable
import org.encryfoundation.common.utils.Algos

final case class InternalNode[K: Hashable, V](key: K,
                                    value: V,
                                    height: Int,
                                    balance: Int,
                                    leftChild: Option[Node[K, V]],
                                    rightChild: Option[Node[K, V]],
                                    hash: Array[Byte]) extends Node[K, V] {

  override def selfInspection: Node[K, V] = if (leftChild.isEmpty & rightChild.isEmpty) LeafNode(key, value)
                                            else this

  def updateChilds(newLeftChild: Option[Node[K, V]] = leftChild,
                   newRightChild: Option[Node[K, V]] = rightChild): InternalNode[K, V] = {
    val newLeftChildAfterInspect = newLeftChild.map(_.selfInspection)
    val newRightChildAfterInspect = newRightChild.map(_.selfInspection)
    this.copy(
      leftChild = newLeftChildAfterInspect,
      rightChild = newRightChildAfterInspect,
      balance = newLeftChildAfterInspect.map(_.height).getOrElse(-1) - newRightChildAfterInspect.map(_.height).getOrElse(-1),
      height = Math.max(newLeftChildAfterInspect.map(_.height).getOrElse(-1), newRightChildAfterInspect.map(_.height).getOrElse(0)) + 1,
      hash = Algos.hash(hash ++
        newLeftChildAfterInspect.map(_.hash).getOrElse(Array.emptyByteArray) ++
        newRightChildAfterInspect.map(_.hash).getOrElse(Array.emptyByteArray))
    )
  }

  override def toString: String = s"[($key," +
    s" $value," +
    s" height: $height," +
    s" balance $balance, " +
    s" hash: ${Algos.encode(hash)}) \n-> LeftChildOf($key):${leftChild.map(_.toString)}, \n-> RightChildOf($key): ${rightChild.map(_.toString)}]"
}

object InternalNode {
  def apply[K, V](key: K,
                  value: V,
                  height: Int,
                  balance: Int)(implicit hash: Hashable[K]): InternalNode[K, V] =
    new InternalNode(key, value, height, balance, None, None, hash = hash.hash(key))
}
