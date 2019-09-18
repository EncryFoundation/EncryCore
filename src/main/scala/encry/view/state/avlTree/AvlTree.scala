package encry.view.state.avlTree

import cats.{Monoid, Order}
import cats.syntax.order._

final case class AvlTree[K : Order, V](rootNode: Node[K, V]) {

  def root: K = rootNode.key

  def insert(k: K, v: V): Node[K, V] = insert(rootNode, k, v)

  private def insert(node: Node[K, V], newKey: K, newValue: V): Node[K, V] = node match {
    case emptyNode: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === newKey) leafNode.copy(value = newValue)
      else {
        val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value)
        insert(newInternalNode, newKey, newValue)
      }
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > newKey) {
        val newLeftChild = internalNode.leftChild.map(previousLeftChild =>
          insert(previousLeftChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        internalNode.copy(leftChild = Some(newLeftChild))
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        internalNode.copy(rightChild = Some(newRightChild))
      }
  }

  override def toString: String = rootNode.toString
}

object AvlTree {
  def apply[K: Monoid : Order, V: Monoid](): AvlTree[K, V] = new AvlTree[K, V](EmptyNode())
}
