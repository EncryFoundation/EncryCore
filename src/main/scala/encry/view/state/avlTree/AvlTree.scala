package encry.view.state.avlTree

import cats.syntax.order._
import cats.{Monoid, Order}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.{EMPTY, LEFT, RIGHT}
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos

import scala.collection.immutable.HashMap
import scala.util.Try

final case class AvlTree[K : Hashable : Order, V](rootNode: Node[K, V], storage: VersionalStorage) extends AutoCloseable {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]]{
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  def root: K = rootNode.key

  val rootHash: Array[Byte] = rootNode.hash

  //contains new or changed nodes. Needs for serialization
  var insertedNodes: Map[ByteArrayWrapper, Node[K, V]] = Map.empty[ByteArrayWrapper, Node[K, V]]

  //contains deleted nodes. Needs for serialization
  var deletedNodes: List[ByteArrayWrapper] = List.empty[ByteArrayWrapper]

  //return newAvl, allUpdatedNodes and hashes of deleted.
  def insertAndDeleteMany(version: StorageVersion,
                          toInsert: List[(K, V)],
                          toDelete: List[K])
                         (implicit kSer: Serializer[K],
                         vSer: Serializer[V],
                         kM: Monoid[K],
                         vM: Monoid[V]): (AvlTree[K, V], Map[ByteArrayWrapper, Node[K, V]], List[ByteArrayWrapper]) = {
    val startTime = System.currentTimeMillis()
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDelete) => delete(toDelete, prevRoot)
    }
    val newRoot = toInsert.foldLeft(rootAfterDelete){
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        insert(prevRoot, keyToInsert, valueToInsert)
    }
    val deletedNodesUpdated = deletedNodes.filterNot(insertedNodes.contains)
    val newInserted = insertedNodes -- deletedNodesUpdated
    val shadowedRoot = ShadowNode.childsToShadowNode(newRoot)
    storage.insert(version,
      toInsert.map{case (key, value) => StorageKey @@ Algos.hash(kSer.toBytes(key)) -> StorageValue @@ vSer.toBytes(value)} ++
        newInserted.values.map(node => StorageKey @@ node.hash -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(node))).toList :+
        (AvlTree.rootNodeKey -> StorageValue @@ shadowedRoot.hash),
      deletedNodesUpdated.map(key => StorageKey @@ key.data)
    )
    (AvlTree(shadowedRoot, storage), insertedNodes, deletedNodes)
  }

  def getOperationsRootHash(toInsert: List[(K, V)],
                            toDelete: List[K])
                           (implicit kSer: Serializer[K],
                            vSer: Serializer[V],
                            kM: Monoid[K],
                            vM: Monoid[V]): Array[Byte] = {
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDelete) => delete(toDelete, prevRoot)
    }
    val newRoot = toInsert.foldLeft(rootAfterDelete){
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        insert(prevRoot, keyToInsert, valueToInsert)
    }
    insertedNodes = Map.empty[ByteArrayWrapper, Node[K, V]]
    deletedNodes = List.empty[ByteArrayWrapper]
    newRoot.hash
  }

  def insert(k: K, v: V): AvlTree[K, V] = {
    val newNode = balance(insert(rootNode, k, v))
    AvlTree(newNode, storage)
  }

  def get(k: K)(implicit kSer: Serializer[K], vSer: Serializer[V]): Option[V] = storage.get(StorageKey !@@ Algos.hash(kSer.toBytes(k))).map(vSer.fromBytes)

  def contains(k: K)(implicit kSer: Serializer[K]): Boolean = storage.get(StorageKey !@@ Algos.hash(kSer.toBytes(k))).isDefined

  def getInTree(k: K): Option[V] = getK(k, rootNode)

  def containsInTree(k: K): Boolean = find(k).isDefined

  def find(k: K): Option[(K, V)] = getK(k, rootNode).map{value =>
    (k, value)}

  private def getK(key: K, node: Node[K, V]): Option[V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      getK(key, restoredNode)
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key === key) Some(internalNode.value)
      else if (internalNode.key > key) internalNode.leftChild.flatMap(child => getK(key, child))
      else internalNode.rightChild.flatMap(child => getK(key, child))
    case leafNode: LeafNode[K, V] => if (leafNode.key === key) Some(leafNode.value) else None
  }

  def delete(key: K, node: Node[K, V])(implicit m: Monoid[K], v: Monoid[V]): Node[K, V] = balance(delete(node, key).get)

  private def delete(node: Node[K, V], key: K)(implicit m: Monoid[K], v: Monoid[V]): Option[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      delete(restoredNode, key)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) None else {
        deletedNodes = ByteArrayWrapper(leafNode.hash) +: deletedNodes
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
            insertedNodes = insertedNodes + (ByteArrayWrapper(newNode.hash) -> newNode)
            newLeftChild.foreach(leftNode => insertedNodes = insertedNodes + (ByteArrayWrapper(leftNode.hash) -> leftNode))
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = internalNode.rightChild.flatMap(node => delete(node, newKey))
            val newNode = internalNode.copy(key = newKey, value = newValue, hash = hash.hash(newKey)).updateChilds(newRightChild = newRightChild)
            insertedNodes = insertedNodes + (ByteArrayWrapper(newNode.hash) -> newNode)
            newRightChild.foreach(rightNode => insertedNodes = insertedNodes + (ByteArrayWrapper(rightNode.hash) -> rightNode))
            newNode
          case ((_, _), EMPTY) => internalNode
        }
        Some(balance(newNode))
      }
  }

  private def findTheClosestValue(node: Node[K, V], key: K)
                                 (implicit m: Monoid[K], v: Monoid[V]): ((K, V), Direction) = {
    val h = implicitly[Order[Node[K, V]]]
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(storage).get
        findTheClosestValue(restoredNode, key)
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
  }

  def rollbackTo[K: Monoid : Serializer : Hashable : Order, V: Monoid : Serializer](to: StorageVersion): Try[AvlTree[K, V]] = Try {
    storage.rollbackTo(to)
    val rootNode = NodeSerilalizer.fromBytes[K, V](storage.get(AvlTree.rootNodeKey).get).get
    AvlTree(rootNode, storage)
  }

  private def getRightPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      getRightPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] => internalNode +: internalNode.rightChild.map(getRightPath).getOrElse(List.empty)
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      getLeftPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] => internalNode +: internalNode.leftChild.map(getLeftPath).getOrElse(List.empty)
  }

  private def insert(node: Node[K, V], newKey: K, newValue: V): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      insert(restoredNode, newKey, newValue)
    case _: EmptyNode[K, V] =>
      val newLeaf = LeafNode[K, V](newKey, newValue)
      addToActionInfo(
        List(ByteArrayWrapper(newLeaf.hash) -> newLeaf),
        List.empty
      )
      newLeaf
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === newKey) leafNode.copy(value = newValue)
      else {
        val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value, height = 1, balance = 0)
        addToActionInfo(
          List(ByteArrayWrapper(newInternalNode.hash) -> newInternalNode),
          List(ByteArrayWrapper(leafNode.hash))
        )
        insert(newInternalNode, newKey, newValue).asInstanceOf[InternalNode[K, V]]
      }
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > newKey) {
        val newLeftChild = internalNode.leftChild.map(previousLeftChild =>
          insert(previousLeftChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val newNode = internalNode.updateChilds(newLeftChild = Some(newLeftChild))
        addToActionInfo(
          List(ByteArrayWrapper(newNode.hash) -> newNode, ByteArrayWrapper(newLeftChild.hash) -> newLeftChild),
          List(ByteArrayWrapper(internalNode.hash))
        )
        val res = balance(newNode)
        res
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val newNode = internalNode.updateChilds(newRightChild = Some(newRightChild))
        addToActionInfo(
          List(ByteArrayWrapper(newNode.hash) -> newNode, ByteArrayWrapper(newRightChild.hash) -> newRightChild),
          List(ByteArrayWrapper(internalNode.hash))
        )
        balance(newNode)
      }
  }

  private def addToActionInfo(toInsert: List[(ByteArrayWrapper, Node[K, V])], toDelete: List[ByteArrayWrapper]): Unit =
    addToActionInfo(HashMap[ByteArrayWrapper, Node[K, V]](toInsert: _*), toDelete)

  private def addToActionInfo(toInsert: HashMap[ByteArrayWrapper, Node[K, V]], toDelete: List[ByteArrayWrapper]): Unit = {
    //val newDeleted = (deletedNodes ::: toDelete).diff(toInsert.keySet.toList)
    insertedNodes ++= toInsert
    deletedNodes :::= toDelete
  }

  private def balance(node: Node[K, V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      balance(restoredNode)
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
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      rightSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.rightChild.map(_.height).getOrElse(-1)
    case _ => -1
  }

  private def leftSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      leftSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.leftChild.map(_.height).getOrElse(-1)
    case _ => -1
  }

  private def rightRotation(node: Node[K, V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      rightRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.leftChild.get match {
        case LeafNode(key, value) => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
        case shadowNode: ShadowNode[K, V] =>
          shadowNode.restoreFullNode(storage).get match {
            case LeafNode(key, value) => InternalNode(key, value, 0, 0)
            case internalNode: InternalNode[K, V] => internalNode
          }
      }
      val newLeftChildForPrevRoot = newRoot.rightChild
      val prevRoot = internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot.map(_.selfInspection)).selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newRightChild = Some(prevRoot.selfInspection))
      addToActionInfo(
        List(ByteArrayWrapper(prevRoot.hash) -> prevRoot, ByteArrayWrapper(newUpdatedRoot.hash) -> newUpdatedRoot),
        List(ByteArrayWrapper(internalNode.hash), ByteArrayWrapper(internalNode.leftChild.get.hash))
      )
      newUpdatedRoot
  }

  private def leftRotation(node: Node[K, V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      leftRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.rightChild.get match {
        case LeafNode(key, value) => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
        case shadowNode: ShadowNode[K, V] =>
          shadowNode.restoreFullNode(storage).get match {
            case LeafNode(key, value) => InternalNode(key, value, 0, 0)
            case internalNode: InternalNode[K, V] => internalNode
          }
      }
      val newRightChildForPrevRoot = newRoot.leftChild
      val prevRoot = internalNode.updateChilds(newRightChild = newRightChildForPrevRoot.map(_.selfInspection)).selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newLeftChild = Some(prevRoot)).selfInspection
      addToActionInfo(
        List(ByteArrayWrapper(newUpdatedRoot.hash) -> newUpdatedRoot, ByteArrayWrapper(prevRoot.hash) -> prevRoot),
        List(ByteArrayWrapper(internalNode.hash), ByteArrayWrapper(internalNode.rightChild.get.hash))
      )
      newUpdatedRoot
  }

  private def rlRotation(node: Node[K, V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      rlRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedRightChild = rightRotation(internalNode.rightChild.get).selfInspection
      leftRotation(internalNode.updateChilds(newRightChild = Some(rotatedRightChild)))
  }

  private def lrRotation(node: Node[K, V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      lrRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedLeftChild = leftRotation(internalNode.leftChild.get).selfInspection
      rightRotation(internalNode.updateChilds(newLeftChild = Some(rotatedLeftChild)))
  }

  override def close(): Unit = storage.close()

  override def toString: String = rootNode.toString
}

object AvlTree {

  def restore[K: Monoid : Serializer : Hashable : Order, V: Monoid : Serializer](storage: VersionalStorage): Try[AvlTree[K, V]] = Try {
    val rootNode = NodeSerilalizer.fromBytes[K, V](storage.get(AvlTree.rootNodeKey).get).get
    AvlTree(rootNode, storage)
  }

  val rootNodeKey: StorageKey = StorageKey !@@ Algos.hash("root_node")

  sealed trait Direction
  object Directions {
    case object LEFT extends Direction
    case object RIGHT extends Direction
    case object EMPTY extends Direction
  }

  def apply[K: Monoid : Order : Hashable, V: Monoid](storage: VersionalStorage): AvlTree[K, V] = new AvlTree[K, V](EmptyNode(), storage)
}
