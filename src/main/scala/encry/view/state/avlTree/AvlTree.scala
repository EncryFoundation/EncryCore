package encry.view.state.avlTree

import cats.kernel.Hash
import cats.{Monoid, Order}
import cats.syntax.order._
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.{EMPTY, LEFT, RIGHT}
import encry.view.state.avlTree.utils.implicits.Hashable

final case class AvlTree[K : Order, V](rootNode: Node[K, V]) {

  implicit def nodeOrder(implicit ord: Order[K]) = new Order[Node[K, V]]{
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

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

  def delete(key: K)(implicit m: Monoid[K], v: Monoid[V]): AvlTree[K, V] = new AvlTree(balance(delete(rootNode, key).get))

  private def delete(node: Node[K, V], key: K)(implicit m: Monoid[K], v: Monoid[V]): Option[Node[K, V]] = node match {
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) None else Some(leafNode)
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > key) {
        val newLeftChild = internalNode.leftChild.flatMap(node => delete(node, key))
        val newNode = internalNode.copy(
          leftChild = newLeftChild,
          balance = newLeftChild.map(_.height).getOrElse(-1) - internalNode.rightChild.map(_.height).getOrElse(-1),
          height = Math.max(newLeftChild.map(_.height).getOrElse(-1), internalNode.rightChild.map(_.height).getOrElse(0)) + 1,
        ).selfInspection
        Some(balance(newNode))
      } else if (internalNode.key < key){
        val newRightChild = internalNode.rightChild.flatMap(node => delete(node, key))
        val newNode = internalNode.copy(
          rightChild = newRightChild,
          balance = internalNode.leftChild.map(_.height).getOrElse(-1) - newRightChild.map(_.height).getOrElse(-1),
          height = Math.max(internalNode.leftChild.map(_.height).getOrElse(0), newRightChild.map(_.height).getOrElse(-1)) + 1,
        ).selfInspection
        Some(balance(newNode))
      } else {
        val theClosestValue = findTheClosestValue(internalNode, internalNode.key)
        println(s"the closest value for: ${internalNode.key} is ${theClosestValue}")
        val newNode = theClosestValue match {
          case ((newKey, newValue), LEFT) =>
            val newLeftChild = internalNode.leftChild.flatMap(node => delete(node, newKey))
            internalNode.copy(
              key = newKey,
              value = newValue,
              leftChild = newLeftChild,
              balance = newLeftChild.map(_.height).getOrElse(-1) - internalNode.rightChild.map(_.height).getOrElse(-1),
              height = Math.max(newLeftChild.map(_.height).getOrElse(0), internalNode.rightChild.map(_.height).getOrElse(0)) + 1
            )
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = internalNode.rightChild.flatMap(node => delete(node, newKey))
            internalNode.copy(
              key = newKey,
              value = newValue,
              rightChild = newRightChild,
              balance = internalNode.leftChild.map(_.height).getOrElse(-1) - newRightChild.map(_.height).getOrElse(-1),
              height = Math.max(internalNode.leftChild.map(_.height).getOrElse(0), newRightChild.map(_.height).getOrElse(-1)) + 1,
            )
          case ((_, _), EMPTY) => internalNode
        }
        Some(balance(newNode))
      }
  }

  private def findTheClosestValue(node: Node[K, V], key: K)
                                 (implicit m: Monoid[K], v: Monoid[V], h: Order[Node[K, V]]): ((K, V), Direction) = node match {
    case leafNode: LeafNode[K, V] => leafNode.key -> leafNode.value -> EMPTY
    case internalNode: InternalNode[K, V] =>
      val onLeft = internalNode.leftChild.map(node => getRightPath(node).foldLeft[Node[K, V]](internalNode){
        case (bestNode, nodeToCompr) => h.min(bestNode, nodeToCompr)
      } -> LEFT)
      onLeft.orElse(
        internalNode.leftChild.map(node => getRightPath(node).foldLeft[Node[K, V]](internalNode){
          case (bestNode, nodeToCompr) => h.max(bestNode, nodeToCompr)
        } -> RIGHT)
      ).map(node => node._1.key -> node._1.value -> node._2).get
  }

  private def getRightPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] => internalNode +: internalNode.rightChild.map(getRightPath).getOrElse(List.empty)
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] => internalNode +: internalNode.leftChild.map(getLeftPath).getOrElse(List.empty)
  }

  private def insert(node: Node[K, V], newKey: K, newValue: V): Node[K, V] = node match {
    case _: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
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
        val node = internalNode.updateChilds(newLeftChild = Some(newLeftChild))
        balance(node)
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val node = internalNode.updateChilds(newRightChild = Some(newRightChild))
        balance(node)
      }
  }

  private def setToZeroInsertDirection(node: Node[K, V]): Node[K, V] = node match {
    case internalNode: InternalNode[K, V] => internalNode.copy(
      leftChild = internalNode.leftChild.map(setToZeroInsertDirection),
      rightChild = internalNode.rightChild.map(setToZeroInsertDirection)
    )
    case leaf => leaf
  }

  private def balance(node: Node[K, V]): Node[K, V] = node match {
    case internalNode: InternalNode[K, V] =>
      val newAdditionalInfo = (
        Math.abs(internalNode.balance),
        internalNode.leftChild.exists(leftChild => rightSubTreeHeight(leftChild) > leftSubTreeHeight(leftChild)), //lr
        internalNode.leftChild.exists(leftChild => rightSubTreeHeight(leftChild) <= leftSubTreeHeight(leftChild)), //r
        internalNode.rightChild.exists(rightChid => leftSubTreeHeight(rightChid) > rightSubTreeHeight(rightChid))
      )
      newAdditionalInfo match {
        case (2, true, _, _) => lrRotation(internalNode)
        case (2, _, true, _) => rightRotation(internalNode)
        case (2, _, _, true) => rlRotation(internalNode)
        case (2, _, _, _) => leftRotation(internalNode)
        case _ => internalNode
      }
    case leafNode: LeafNode[K, V] => leafNode
  }

  private def rightSubTreeHeight(node: Node[K, V]): Int = node match {
    case internalNode: InternalNode[K, V] => internalNode.rightChild.map(_.height).getOrElse(-1)
    case _ => -1
  }

  private def leftSubTreeHeight(node: Node[K, V]): Int = node match {
    case internalNode: InternalNode[K, V] => internalNode.leftChild.map(_.height).getOrElse(-1)
    case _ => -1
  }

  private def rightRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.leftChild.get match {
        case LeafNode(key, value) => InternalNode(key, value, 0, 0)
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
        case LeafNode(key, value) => InternalNode(key, value, 0, 0)
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

  sealed trait Direction
  object Directions {
    case object LEFT extends Direction
    case object RIGHT extends Direction
    case object EMPTY extends Direction
  }

  def apply[K: Monoid : Order, V: Monoid](): AvlTree[K, V] = new AvlTree[K, V](EmptyNode())
}
