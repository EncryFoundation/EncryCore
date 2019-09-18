package encry.view.state.avlTree

import cats.{Monoid, Order}
import cats.syntax.order._

final case class AvlTree[K : Order, V](rootNode: Node[K, V]) {

  def root: K = rootNode.key

  def insert(k: K, v: V): AvlTree[K, V] = {
    val newNode = insert(rootNode, k, v)
//    if (newNode.balance == 2) AvlTree(leftRotation(newNode))
//    else
      AvlTree(newNode)
  }

  private def insert(node: Node[K, V], newKey: K, newValue: V): Node[K, V] = node match {
    case emptyNode: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === newKey) leafNode.copy(value = newValue)
      else {
        val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value, height = 1, balance = 0)
        //todo remove asInstance
        val newNode = insert(newInternalNode, newKey, newValue).asInstanceOf[InternalNode[K, V]]
        newNode.copy(balance = newNode.leftChild.map(_.height).getOrElse(-1) - newNode.rightChild.map(_.height).getOrElse(-1))
      }
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > newKey) {
        val newLeftChild = internalNode.leftChild.map(previousLeftChild =>
          insert(previousLeftChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        internalNode.copy(
          leftChild = Some(newLeftChild),
          height = Math.max(newLeftChild.height, internalNode.rightChild.map(_.height).getOrElse(0)) + 1,
          balance = newLeftChild.height - internalNode.rightChild.map(_.height).getOrElse(-1)
        )
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        internalNode.copy(
          rightChild = Some(newRightChild),
          height = Math.max(internalNode.leftChild.map(_.height).getOrElse(0), newRightChild.height) + 1,
          balance = internalNode.leftChild.map(_.height).getOrElse(-1) - newRightChild.height
        )
      }
  }

  private def rightRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.leftChild.get.asInstanceOf[InternalNode[K, V]]
      val newLeftChildForPrevRoot = newRoot.rightChild
      val prevRoot = internalNode.copy(
        leftChild = newLeftChildForPrevRoot,
        height = Math.max(
          newLeftChildForPrevRoot.map(_.height).getOrElse(1),
          internalNode.rightChild.map(_.height).getOrElse(1)
        ) + 1,
        balance = newLeftChildForPrevRoot.map(_.balance).getOrElse(-1) - internalNode.rightChild.map(_.balance).getOrElse(-1)
      )
      newRoot.copy(
        rightChild = Some(prevRoot),
        height = Math.max(
          newRoot.leftChild.map(_.height).getOrElse(1),
          prevRoot.height
        ) + 1,
        balance = newRoot.leftChild.map(_.balance).getOrElse(-1) - prevRoot.balance
      )
  }

  private def leftRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.rightChild.get.asInstanceOf[InternalNode[K, V]]
      val newRightChildForPrevRoot = newRoot.leftChild
      val prevRoot = internalNode.copy(
        rightChild = newRightChildForPrevRoot,
        height = Math.max(
          newRightChildForPrevRoot.map(_.height).getOrElse(1),
          internalNode.leftChild.map(_.height).getOrElse(1)
        ) + 1,
      )
      newRoot.copy(
        leftChild = Some(prevRoot),
        height = Math.max(
          prevRoot.height,
          newRoot.rightChild.map(_.height).getOrElse(1)
        ) + 1,
        balance = prevRoot.balance - newRoot.rightChild.map(_.balance).getOrElse(-1)
      )
  }

  override def toString: String = rootNode.toString
}

object AvlTree {
  def apply[K: Monoid : Order, V: Monoid](): AvlTree[K, V] = new AvlTree[K, V](EmptyNode())
}
