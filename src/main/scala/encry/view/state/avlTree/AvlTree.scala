package encry.view.state.avlTree

import cats.syntax.order._
import cats.{Monoid, Order}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.{EMPTY, LEFT, RIGHT}
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import org.encryfoundation.common.utils.Algos
import scorex.crypto.hash.Digest32
import scorex.utils.Random

import scala.collection.immutable.HashMap

final case class AvlTree[K : Hashable : Order, V](rootNode: Node[K, V], storage: VersionalStorage) {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]]{
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  def root: K = rootNode.key

  val rootHash: Array[Byte] = rootNode.hash

  //contains new or changed nodes. Needs for serialization
  var insertedNodes: Map[String, Node[K, V]] = Map.empty[String, Node[K, V]]

  //contains deleted nodes. Needs for serialization
  var deletedNodes: List[String] = List.empty[String]

  //return newAvl, allUpdatedNodes and hashes of deleted.
  def insertMany(toInsert: List[(K, V)])
                (implicit kSer: Serializer[K],
                 vSer: Serializer[V],
                 kM: Monoid[K],
                 vM: Monoid[V]): (AvlTree[K, V], Map[String, Node[K, V]], List[String]) = {
    val startTime = System.currentTimeMillis()
    val newRoot = toInsert.foldLeft(rootNode){
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        setToZeroInsertDirection(insert(prevRoot, keyToInsert, valueToInsert))
    }
    println(s"new root getting: ${(System.currentTimeMillis() - startTime)/1000L} s")
    val deletedNodesUpdated = deletedNodes.filterNot(insertedNodes.contains)
    val newInserted = insertedNodes -- deletedNodesUpdated
    val shadowedRoot = ShadowNode.childsToShadowNode(newRoot)
    storage.insert(StorageVersion @@ Random.randomBytes(),
      toInsert.map{case (key, value) => StorageKey @@ Algos.hash(kSer.toBytes(key)) -> StorageValue @@ vSer.toBytes(value)} ++
        newInserted.values.map(node => StorageKey @@ node.hash -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(node))).toList :+
        (AvlTree.rootNodeKey -> StorageValue @@ shadowedRoot.hash),
      deletedNodesUpdated.map(key => StorageKey @@ Algos.decode(key).get)
    )
    (AvlTree(shadowedRoot, storage), insertedNodes, deletedNodes)
  }

  def insert(k: K, v: V): AvlTree[K, V] = {
    val newNode = balance(insert(rootNode, k, v))
    AvlTree(setToZeroInsertDirection(newNode), storage)
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

  def delete(key: K)(implicit m: Monoid[K], v: Monoid[V]): AvlTree[K, V] = new AvlTree(balance(delete(rootNode, key).get), storage)

  private def delete(node: Node[K, V], key: K)(implicit m: Monoid[K], v: Monoid[V]): Option[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      delete(restoredNode, key)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) None else {
        deletedNodes = Algos.encode(leafNode.hash) +: deletedNodes
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
            insertedNodes = insertedNodes + (Algos.encode(newNode.hash) -> newNode)
            newLeftChild.foreach(leftNode => insertedNodes = insertedNodes + (Algos.encode(leftNode.hash) -> leftNode))
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = internalNode.rightChild.flatMap(node => delete(node, newKey))
            val newNode = internalNode.copy(key = newKey, value = newValue, hash = hash.hash(newKey)).updateChilds(newRightChild = newRightChild)
            insertedNodes = insertedNodes + (Algos.encode(newNode.hash) -> newNode)
            newRightChild.foreach(rightNode => insertedNodes = insertedNodes + (Algos.encode(rightNode.hash) -> rightNode))
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
        List(Algos.encode(newLeaf.hash) -> newLeaf),
        List.empty
      )
      newLeaf
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === newKey) leafNode.copy(value = newValue)
      else {
        val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value, height = 1, balance = 0)
        addToActionInfo(
          List(Algos.encode(newInternalNode.hash) -> newInternalNode),
          List(Algos.encode(leafNode.hash))
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
          List(Algos.encode(newNode.hash) -> newNode, Algos.encode(newLeftChild.hash) -> newLeftChild),
          List(Algos.encode(internalNode.hash))
        )
        val res = balance(newNode)
        res
      } else {
        val newRightChild = internalNode.rightChild.map(previousRightChild =>
          insert(previousRightChild, newKey, newValue)
        ).getOrElse(LeafNode(newKey, newValue))
        val newNode = internalNode.updateChilds(newRightChild = Some(newRightChild))
        addToActionInfo(
          List(Algos.encode(newNode.hash) -> newNode, Algos.encode(newRightChild.hash) -> newRightChild),
          List(Algos.encode(internalNode.hash))
        )
        balance(newNode)
      }
  }

  private def addToActionInfo(toInsert: List[(String, Node[K, V])], toDelete: List[String]): Unit =
    addToActionInfo(HashMap[String, Node[K, V]](toInsert: _*), toDelete)

  private def addToActionInfo(toInsert: HashMap[String, Node[K, V]], toDelete: List[String]): Unit = {
    //val newDeleted = (deletedNodes ::: toDelete).diff(toInsert.keySet.toList)
    insertedNodes ++= toInsert
    deletedNodes :::= toDelete
  }

  private def setToZeroInsertDirection(node: Node[K, V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage).get
      setToZeroInsertDirection(restoredNode)
    case internalNode: InternalNode[K, V] =>
      val newNode = internalNode.updateChilds(
        newLeftChild = internalNode.leftChild.map(setToZeroInsertDirection),
        newRightChild = internalNode.rightChild.map(setToZeroInsertDirection)
      )
      addToActionInfo(
        List(Algos.encode(newNode.hash) -> newNode),
        List(Algos.encode(internalNode.hash))
      )
      newNode
    case leaf => leaf
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
      }
      val newLeftChildForPrevRoot = newRoot.rightChild
      val prevRoot = internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot.map(_.selfInspection)).selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newRightChild = Some(prevRoot.selfInspection))
      addToActionInfo(
        List(Algos.encode(prevRoot.hash) -> prevRoot, Algos.encode(newUpdatedRoot.hash) -> newUpdatedRoot),
        List(Algos.encode(internalNode.hash), Algos.encode(internalNode.leftChild.get.hash))
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
      }
      val newRightChildForPrevRoot = newRoot.leftChild
      val prevRoot = internalNode.updateChilds(newRightChild = newRightChildForPrevRoot.map(_.selfInspection)).selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newLeftChild = Some(prevRoot)).selfInspection
      addToActionInfo(
        List(Algos.encode(newUpdatedRoot.hash) -> newUpdatedRoot, Algos.encode(prevRoot.hash) -> prevRoot),
        List(Algos.encode(internalNode.hash), Algos.encode(internalNode.rightChild.get.hash))
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

  override def toString: String = rootNode.toString
}

object AvlTree {

  val rootNodeKey: StorageKey = StorageKey !@@ Algos.hash("root_node")

  sealed trait Direction
  object Directions {
    case object LEFT extends Direction
    case object RIGHT extends Direction
    case object EMPTY extends Direction
  }

  def apply[K: Monoid : Order : Hashable, V: Monoid](storage: VersionalStorage): AvlTree[K, V] = new AvlTree[K, V](EmptyNode(), storage)
}
