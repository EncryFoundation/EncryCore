package encry.view.state.avlTree

import cats.{Monoid, Order}
import cats.syntax.order._
import encry.view.state.avlTree.AvlTree.InsertDirections.{EMPTY, LEFT, RIGHT}

final case class AvlTree[K : Order, V](rootNode: Node[K, V]) {

  def root: K = rootNode.key

  def insert(k: K, v: V): AvlTree[K, V] = {
    val newNode = balance(insert(rootNode, k, v))
    AvlTree(setToZeroInsertDirection(newNode))
  }

  def get(k: K): Option[V] = getK(k, rootNode)

  def contains(k: K): Boolean = getK(k, rootNode).isDefined

  def find(k: K): Option[(K, V)] = getK(k, rootNode).map(value => (k, value))

  private def getK(key: K, node: Node[K, V]): Option[V] = node match {
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key === key) Some(internalNode.value)
      else if (internalNode.key > key) internalNode.leftChild.flatMap(child => getK(key, child))
      else internalNode.rightChild.flatMap(child => getK(key, child))
    case leafNode: LeafNode[K, V] => if (leafNode.key === key) Some(leafNode.value) else None
  }

  private def insert(node: Node[K, V], newKey: K, newValue: V): Node[K, V] = node match {
    case emptyNode: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === newKey) leafNode.copy(value = newValue)
      else {
        val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value, height = 1, balance = 0, EMPTY)
        //todo remove asInstance
        val newNode = insert(newInternalNode, newKey, newValue).asInstanceOf[InternalNode[K, V]]
        newNode.copy(balance = newNode.leftChild.map(_.height).getOrElse(-1) - newNode.rightChild.map(_.height).getOrElse(-1))
      }
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > newKey) {
        val newLeftChild = internalNode.leftChild.map(previousLeftChild =>
          insert(previousLeftChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val node = internalNode.copy(
          leftChild = Some(newLeftChild),
          rightChild = internalNode.rightChild.map(node => node.selfInspection),
          insertDirection = LEFT,
          height = Math.max(newLeftChild.height, internalNode.rightChild.map(_.height).getOrElse(0)) + 1,
          balance = newLeftChild.height - internalNode.rightChild.map(_.height).getOrElse(-1)
        )
        balance(node)
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val node =
          internalNode.copy(
            rightChild = Some(newRightChild),
            insertDirection = RIGHT,
            leftChild = internalNode.leftChild.map(node => node.selfInspection),
            height = Math.max(internalNode.leftChild.map(_.height).getOrElse(0), newRightChild.height) + 1,
            balance = internalNode.leftChild.map(_.height).getOrElse(-1) - newRightChild.height
          )
        balance(node)
      }
  }

  private def setToZeroInsertDirection(node: Node[K, V]): Node[K, V] = node match {
    case internalNode: InternalNode[K, V] => internalNode.copy(
      insertDirection = EMPTY,
      leftChild = internalNode.leftChild.map(setToZeroInsertDirection),
      rightChild = internalNode.rightChild.map(setToZeroInsertDirection)
    )
    case leaf => leaf
  }

  private def balance(node: Node[K, V]): Node[K, V] = node match {
    case internalNode: InternalNode[K, V] =>
      val additionalInfo = (internalNode.balance,
        internalNode.insertDirection,
        internalNode.leftChild.map(_.insertDirection),
        internalNode.rightChild.map(_.insertDirection))
      additionalInfo match {
        case (2, LEFT, Some(LEFT), Some(EMPTY)) => rightRotation(internalNode)
        case (2, LEFT, Some(RIGHT), Some(EMPTY)) => lrRotation(internalNode)
        case (-2, RIGHT, Some(EMPTY), Some(RIGHT)) => leftRotation(internalNode)
        case (-2, RIGHT, Some(EMPTY), Some(LEFT)) => rlRotation(internalNode)
        case _ => internalNode
      }
    case leafNode: LeafNode[K, V] => leafNode
  }

  private def rightRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.leftChild.get match {
        case LeafNode(key, value) => InternalNode(key, value, 0, 0, EMPTY)
        case internalNode: InternalNode[K, V] => internalNode
      }
      val newLeftChildForPrevRoot = newRoot.rightChild
      val prevRoot = internalNode.copy(
        leftChild = newLeftChildForPrevRoot.map(_.selfInspection),
        height = Math.max(
          newLeftChildForPrevRoot.map(_.height).getOrElse(-1),
          internalNode.rightChild.map(_.height).getOrElse(-1)
        ) + 1,
        balance = newLeftChildForPrevRoot.map(_.height).getOrElse(-1) - internalNode.rightChild.map(_.height).getOrElse(-1)
      )
      //println(s"prevroot: ${prevRoot}")
      newRoot.copy(
        rightChild = Some(prevRoot.selfInspection),
        height = Math.max(
          newRoot.leftChild.map(_.height).getOrElse(-1),
          prevRoot.height
        ) + 1,
        balance = newRoot.leftChild.map(_.height).getOrElse(-1) - prevRoot.balance
      )
  }

  private def leftRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.rightChild.get match {
        case LeafNode(key, value) => InternalNode(key, value, 0, 0, EMPTY)
        case internalNode: InternalNode[K, V] => internalNode
      }
      val newRightChildForPrevRoot = newRoot.leftChild
      val prevRoot = internalNode.copy(
        rightChild = newRightChildForPrevRoot.map(_.selfInspection),
        height = Math.max(
          newRightChildForPrevRoot.map(_.height).getOrElse(-1),
          internalNode.leftChild.map(_.height).getOrElse(-1)
        ) + 1,
      ).selfInspection
      newRoot.copy(
        leftChild = Some(prevRoot),
        height = Math.max(
          prevRoot.height,
          newRoot.rightChild.map(_.height).getOrElse(-1)
        ) + 1,
        balance = prevRoot.height - newRoot.rightChild.map(_.height).getOrElse(-1)
      ).selfInspection
  }

  private def rlRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedRightChild = rightRotation(internalNode.rightChild.get).selfInspection
      leftRotation(internalNode.copy(rightChild = Some(rotatedRightChild)))
  }

  private def lrRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedLeftChild = leftRotation(internalNode.leftChild.get).selfInspection
      rightRotation(internalNode.copy(leftChild = Some(rotatedLeftChild)))
  }

  override def toString: String = rootNode.toString
}

object AvlTree {

  sealed trait InsertDirection
  object InsertDirections {
    case object LEFT extends InsertDirection
    case object RIGHT extends InsertDirection
    case object EMPTY extends InsertDirection
  }

  def apply[K: Monoid : Order, V: Monoid](): AvlTree[K, V] = new AvlTree[K, V](EmptyNode())
}
