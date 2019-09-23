package encry.view.state.avlTree

import java.util

import cats.kernel.Hash
import cats.{Monoid, Order}
import cats.syntax.order._
import cats.syntax.option._
import encry.storage.VersionalStorage
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.{EMPTY, LEFT, RIGHT}
import encry.view.state.avlTree.utils.implicits.Hashable
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable

final case class AvlTree[K : Order : Hashable, V](rootNode: Node[K, V], storage: VersionalStorage) {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]]{
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  def root: K = rootNode.key

  //contains new or changed nodes. Needs for serialization
  var insertedNodes: HashMap[String, Node[K, V]] = HashMap.empty[String, Node[K, V]]

  //contains deleted nodes. Needs for serialization
  var deletedNodes: HashSet[String] = HashSet.empty[String]

  //return newAvl, allUpdatedNodes and hashes of deleted.
  def insertMany(toInsert: List[(K, V)]): (AvlTree[K, V], HashMap[String, Node[K, V]], HashSet[String]) = {
    val newRoot = toInsert.foldLeft(rootNode){
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        setToZeroInsertDirection(balance(insert(prevRoot, keyToInsert, valueToInsert)))
    }
    (AvlTree(newRoot, storage), insertedNodes, deletedNodes)
  }

  def insert(k: K, v: V): AvlTree[K, V] = {
    val newNode = balance(insert(rootNode, k, v))
    AvlTree(setToZeroInsertDirection(newNode), storage)
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

  def delete(key: K)(implicit m: Monoid[K], v: Monoid[V]): AvlTree[K, V] = new AvlTree(balance(delete(rootNode, key).get), storage)

  private def delete(node: Node[K, V], key: K)(implicit m: Monoid[K], v: Monoid[V]): Option[Node[K, V]] = node match {
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) None else {
        deletedNodes = deletedNodes + util.Arrays.toString(leafNode.hash)
        Some(leafNode)
      }
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > key) {
        val newLeftChild = internalNode.leftChild.flatMap(node => delete(node, key))
        val newNode = internalNode.updateChilds(newLeftChild = newLeftChild).selfInspection
        Some(balance(newNode))
      } else if (internalNode.key < key){
        val newRightChild = internalNode.rightChild.flatMap(node => delete(node, key))
        val newNode = internalNode.updateChilds(newRightChild = newRightChild).selfInspection
        Some(balance(newNode))
      } else {
        val theClosestValue = findTheClosestValue(internalNode, internalNode.key)
        val hash = implicitly[Hashable[K]]
        val newNode = theClosestValue match {
          case ((newKey, newValue), LEFT) =>
            val newLeftChild = internalNode.leftChild.flatMap(node => delete(node, newKey))
            val newNode = internalNode.copy(key = newKey, value = newValue, hash = hash.hash(newKey)).updateChilds(newLeftChild = newLeftChild)
            insertedNodes = insertedNodes + (util.Arrays.toString(newNode.hash) -> newNode)
            newLeftChild.foreach(leftNode => insertedNodes = insertedNodes + (util.Arrays.toString(leftNode.hash) -> leftNode))
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = internalNode.rightChild.flatMap(node => delete(node, newKey))
            val newNode = internalNode.copy(key = newKey, value = newValue, hash = hash.hash(newKey)).updateChilds(newRightChild = newRightChild)
            insertedNodes = insertedNodes + (util.Arrays.toString(newNode.hash) -> newNode)
            newRightChild.foreach(rightNode => insertedNodes = insertedNodes + (util.Arrays.toString(rightNode.hash) -> rightNode))
            newNode
          case ((_, _), EMPTY) => internalNode
        }
        Some(balance(newNode))
      }
  }

  private def findTheClosestValue(node: Node[K, V], key: K)
                                 (implicit m: Monoid[K], v: Monoid[V], h: Order[Node[K, V]]): ((K, V), Direction) = node match {
    case leafNode: LeafNode[K, V] => leafNode.key -> leafNode.value -> EMPTY
    case internalNode: InternalNode[K, V] =>
      val onLeft = internalNode.leftChild.flatMap{node =>
        val rightPath = getRightPath(node)
        rightPath.headOption.map(head =>
          rightPath.foldLeft[Node[K, V]](head){
            case (bestNode, nodeToCompr) => h.max(bestNode, nodeToCompr)
          } -> LEFT
        ) }
      onLeft.orElse {
        val onRight = internalNode.rightChild.flatMap{node =>
          val leftPath = getLeftPath(node)
            leftPath.headOption.map(head =>
              leftPath.foldLeft[Node[K, V]](head) {
                case (bestNode, nodeToCompr) => h.min(bestNode, nodeToCompr)
              } -> RIGHT)
        }
        onRight
      }.map(node => node._1.key -> node._1.value -> node._2).get
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
        insertedNodes = insertedNodes + (util.Arrays.toString(newInternalNode.hash) -> newInternalNode)
        insert(newInternalNode, newKey, newValue).asInstanceOf[InternalNode[K, V]]
      }
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > newKey) {
        val newLeftChild = internalNode.leftChild.map(previousLeftChild =>
          insert(previousLeftChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val node = internalNode.updateChilds(newLeftChild = Some(newLeftChild))
        insertedNodes = insertedNodes + (util.Arrays.toString(node.hash) -> node) + (util.Arrays.toString(newLeftChild.hash) -> newLeftChild)
        balance(node)
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val node = internalNode.updateChilds(newRightChild = Some(newRightChild))
        insertedNodes = insertedNodes + (util.Arrays.toString(node.hash) -> node) + (util.Arrays.toString(newRightChild.hash) -> newRightChild)
        balance(node)
      }
  }

  private def setToZeroInsertDirection(node: Node[K, V]): Node[K, V] = node match {
    case internalNode: InternalNode[K, V] => internalNode.updateChilds(
      newLeftChild = internalNode.leftChild.map(setToZeroInsertDirection),
      newRightChild = internalNode.rightChild.map(setToZeroInsertDirection)
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
      val prevRoot = internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot.map(_.selfInspection))
      //println(s"prevroot: ${prevRoot}")
      val newUpdatedRoot = newRoot.updateChilds(newRightChild = Some(prevRoot.selfInspection))
      insertedNodes = insertedNodes + (util.Arrays.toString(prevRoot.hash) -> prevRoot) + (util.Arrays.toString(newUpdatedRoot.hash) -> newUpdatedRoot)
      deletedNodes = deletedNodes + util.Arrays.toString(internalNode.hash) + util.Arrays.toString(internalNode.leftChild.get.hash)
      newUpdatedRoot
  }

  private def leftRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.rightChild.get match {
        case LeafNode(key, value) => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
      }
      val newRightChildForPrevRoot = newRoot.leftChild
      val prevRoot = internalNode.updateChilds(newRightChild = newRightChildForPrevRoot.map(_.selfInspection)).selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newLeftChild = Some(prevRoot)).selfInspection
      insertedNodes = insertedNodes + (util.Arrays.toString(prevRoot.hash) -> prevRoot) + (util.Arrays.toString(newUpdatedRoot.hash) -> newUpdatedRoot)
      deletedNodes = deletedNodes + util.Arrays.toString(internalNode.hash) + util.Arrays.toString(internalNode.rightChild.get.hash)
      newUpdatedRoot
  }

  private def rlRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedRightChild = rightRotation(internalNode.rightChild.get).selfInspection
      leftRotation(internalNode.updateChilds(newRightChild = Some(rotatedRightChild)))
  }

  private def lrRotation(node: Node[K, V]): Node[K, V] = node match {
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedLeftChild = leftRotation(internalNode.leftChild.get).selfInspection
      rightRotation(internalNode.updateChilds(newLeftChild = Some(rotatedLeftChild)))
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

  def apply[K: Monoid : Order : Hashable, V: Monoid](storage: VersionalStorage): AvlTree[K, V] = new AvlTree[K, V](EmptyNode(), storage)
}
