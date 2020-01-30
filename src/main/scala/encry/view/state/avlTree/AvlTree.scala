package encry.view.state.avlTree

import cats.syntax.order._
import cats.{Monoid, Order}
import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.stats.StatsSender.AvlStat
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.fast.sync.SnapshotHolder.SnapshotChunk
import encry.view.fast.sync.SnapshotHolder.SnapshotManifest.ChunkId
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.{EMPTY, LEFT, RIGHT}
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import scala.util.Try

final case class AvlTree[K : Hashable : Order, V](rootNode: Node[K, V],
                                                  avlStorage: VersionalStorage,
                                                  rootNodes: List[(Node[K, V], StorageVersion)] = List.empty) extends AutoCloseable with StrictLogging {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]] {
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  def root: K = rootNode.key

  val rootHash: Array[Byte] = rootNode.hash

  var nodesBuffer: List[Node[K, V]] = List.empty
  var nodesInsertionStat: List[(ByteArrayWrapper, Int)] = List.empty
  var loggable: Boolean = false
  val rootNodesRollbackLength = 30

  def insertAndDeleteMany(version: StorageVersion,
                          toInsert: List[(K, V)],
                          toDelete: List[K],
                          stateHeight: Height = Height @@ 0)
                         (implicit kSer: Serializer[K],
                         vSer: Serializer[V],
                         kM: Monoid[K],
                         vM: Monoid[V]): AvlTree[K, V] = {
    if (stateHeight > 40000) loggable = true
    val deleteStartTime = System.nanoTime()
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDeleteKey) =>
        val res = deleteKey(toDeleteKey, prevRoot)
        res
    }
    val avlDeleteTime = System.nanoTime() - deleteStartTime
    logger.info(s"avlDeleteTime: ${avlDeleteTime/1000000L} ms")
    val insertStartTime = System.nanoTime()
    val newRoot = toInsert.foldLeft(rootAfterDelete) {
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        if (loggable) logger.info(s"Insert: ${Algos.encode(implicitly[Serializer[K]].toBytes(keyToInsert))}")
        val res = insert(keyToInsert, valueToInsert, prevRoot)
        res
    }
    val insertTime = System.nanoTime() - insertStartTime
    logger.info(s"avlInsertTime: ${insertTime/1000000L} ms")
    //val shadowedRoot    = ShadowNode.childsToShadowNode(newRoot)
    val startPackingTime = System.nanoTime()
//    var insertedNodes: Map[ByteArrayWrapper, Node[K, V]] = Map.empty
//    var deletedNodes: List[ByteArrayWrapper] = List.empty
//    val nodeInsertionMap = nodesInsertionStat.reverse.toMap
//    nodesBuffer.foreach { node =>
//      val wrappedHash = ByteArrayWrapper(node.hash)
//      val stat = nodeInsertionMap(wrappedHash)
//      if (stat >= 0 && wrappedHash.size != 0) {
//        insertedNodes = insertedNodes + (wrappedHash -> node)
//      }
//      else if (wrappedHash.size != 0) deletedNodes = wrappedHash :: deletedNodes
//    }
    logger.info(s"Packing time: ${(System.nanoTime() - startPackingTime)/1000000} ms")
    val startInsertTime = System.nanoTime()
    //val nodesToDelete: List[ByteArrayWrapper] = deletedNodes.toSet[ByteArrayWrapper].filterNot(insertedNodes.keySet).toList
    logger.info(s"Insert in avl version ${Algos.encode(version)}")
    avlStorage.insert(
      version,
      toInsert.map {
        case (key, value) =>
          StorageKey @@ AvlTree.elementKey(kSer.toBytes(key)) -> StorageValue @@ vSer.toBytes(value)
      } ++
//        insertedNodes.map {
//          case (key, node) =>
//            StorageKey @@ AvlTree.nodeKey(key.data) -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(node))
//        }.toList ++
        List(AvlTree.rootNodeKey -> StorageValue @@ newRoot.hash,
          UtxoState.bestHeightKey -> StorageValue @@ Ints.toByteArray(stateHeight)),
//      nodesToDelete.distinct.collect {
//        case key if avlStorage.contains(StorageKey @@ AvlTree.nodeKey(key.data)) =>
//          StorageKey @@ AvlTree.nodeKey(key.data)
//      } ++
        toDelete.map(key =>
        StorageKey @@ AvlTree.elementKey(kSer.toBytes(key))
      )
    )
    logger.info(s"Insertion time: ${(System.nanoTime() - startInsertTime)/1000000L} ms")
    val newrootNodes = if (rootNodes.length > rootNodesRollbackLength) {
      rootNodes.dropRight(rootNodes.length - rootNodesRollbackLength)
    } else (rootNode -> version) :: rootNodes
    AvlTree(newRoot, avlStorage, newrootNodes)
  }

  def getOperationsRootHash(
    toInsert: List[(K, V)],
    toDelete: List[K]
  )(implicit kSer: Serializer[K], vSer: Serializer[V], kM: Monoid[K], vM: Monoid[V]): Try[Array[Byte]] = Try {
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDelete) =>
        deleteKey(toDelete, prevRoot)
    }
    val newRoot = toInsert.foldLeft(rootAfterDelete) {
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        insert(keyToInsert, valueToInsert, prevRoot)
    }
    newRoot.hash
  }

  def get(k: K)(implicit kSer: Serializer[K], vSer: Serializer[V]): Option[V] =
    avlStorage.get(StorageKey !@@ AvlTree.elementKey(kSer.toBytes(k))).map(vSer.fromBytes)

  def contains(k: K)(implicit kSer: Serializer[K]): Boolean =
    avlStorage.get(StorageKey !@@ AvlTree.elementKey(kSer.toBytes(k))).isDefined

  def addToStat(inserted: List[Node[K, V]] = List.empty, deleted: List[Node[K, V]] = List.empty): Unit = {
//    nodesBuffer = inserted ::: nodesBuffer
//    nodesInsertionStat = deleted.map { node =>
//      val wrappedNodeHash = ByteArrayWrapper(node.hash)
//      wrappedNodeHash -> -1
//    } ::: nodesInsertionStat
//    nodesInsertionStat = inserted.map { node =>
//      val wrappedNodeHash = ByteArrayWrapper(node.hash)
//      wrappedNodeHash -> 1
//    } ::: nodesInsertionStat
  }

  def addToStat(insert: Node[K, V], deleted: Node[K, V]): Unit = addToStat(List(insert), List(deleted))

  def deleteKey(key: K, node: Node[K, V])(implicit m: Monoid[K],
                                          v: Monoid[V],
                                          kSer: Serializer[K],
                                          vSer: Serializer[V]): Node[K, V] = delete(node, key)

  private def delete(node: Node[K, V], key: K)(
    implicit m: Monoid[K],
    v: Monoid[V],
    kSer: Serializer[K],
    vSer: Serializer[V]
  ): Node[K, V] = node match {
    case emptyNode: EmptyNode[K, V] => emptyNode
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      delete(restoredNode, key)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) {
        addToStat(deleted = leafNode :: Nil)
        EmptyNode[K, V]
      }
      else leafNode
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key > key) {
        val newLeftChild = delete(internalNode.leftChild, key)
        val childUpdated = internalNode.updateChilds(newLeftChild = newLeftChild)
        val deletedNodes = List(internalNode.leftChild, internalNode)
        val newNodes     = List(newLeftChild, childUpdated)
        addToStat(newNodes, deletedNodes)
        balance(childUpdated)
      } else if (internalNode.key < key) {
        val newRightChild = delete(internalNode.rightChild, key)
        val childUpdated = internalNode.updateChilds(newRightChild = newRightChild)
        val deletedNodes = List(internalNode.rightChild, internalNode)
        val newNodes     = List(newRightChild, childUpdated)
        addToStat(newNodes, deletedNodes)
        balance(childUpdated)
      } else {
        val theClosestValue = findTheClosestValue(internalNode, internalNode.key)
        val newNode = theClosestValue match {
          case ((newKey, newValue), LEFT) =>
            val newLeftChild = delete(internalNode.leftChild, newKey)
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newLeftChild = newLeftChild)
            val deletedNodes = List(internalNode.leftChild, internalNode)
            val newNodes     = List(newNode, newLeftChild)
            addToStat(newNodes, deletedNodes)
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = delete(internalNode.rightChild, newKey)
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newRightChild = newRightChild)
            val deletedNodes = List(internalNode.rightChild, internalNode)
            val newNodes     = List(newNode, newRightChild)
            addToStat(newNodes, deletedNodes)
            newNode
          case ((_, _), EMPTY) => internalNode
        }
        balance(newNode)
      }
  }

  private def findTheClosestValue(node: Node[K, V], key: K)(implicit m: Monoid[K],
                                                            v: Monoid[V]): ((K, V), Direction) = {
    val h = implicitly[Order[Node[K, V]]]
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        findTheClosestValue(restoredNode, key)
      case leafNode: LeafNode[K, V] => leafNode.key -> leafNode.value -> EMPTY
      case internalNode: InternalNode[K, V] =>
        val onLeft = {
          val rightPath = getRightPath(internalNode.leftChild)
          rightPath.headOption.map(
            head =>
              rightPath.foldLeft[Node[K, V]](head) {
                case (bestNode, nodeToCompr) => h.max(bestNode, nodeToCompr)
              } -> LEFT
          )
        }
        val res = onLeft.orElse {
          val onRight = {
            val leftPath = getLeftPath(internalNode.rightChild)
            leftPath.headOption.map(
              head =>
                leftPath.foldLeft[Node[K, V]](head) {
                  case (bestNode, nodeToCompr) => h.min(bestNode, nodeToCompr)
                } -> RIGHT
            )
          }
          onRight
        }.map(node => node._1.key -> node._1.value -> node._2)
        if (res.isEmpty) println(internalNode)
        res.get
    }
  }

  def rollbackTo(to: StorageVersion)
                (implicit kMonoid: Monoid[K], kSer: Serializer[K], vMonoid: Monoid[V], vSer: Serializer[V]): Try[AvlTree[K, V]] =
    Try {
      logger.info(s"Rollback avl to version: ${Algos.encode(to)}")
      logger.info(s"Versions in storage: ${avlStorage.versions.map(Algos.encode).mkString(",")}")
      logger.info(s"Before rollback node key: ${Algos.encode(avlStorage.get(AvlTree.rootNodeKey).get)}")
      logger.info(s"Before rollback root node: ${rootNode.hash}")
      avlStorage.rollbackTo(to)
      logger.info(s"Storage success rolled back")
      logger.info(s"rootNodeKey: ${Algos.encode(avlStorage.get(AvlTree.rootNodeKey).get)}")
//      val newRootNode =
//        NodeSerilalizer.fromBytes[K, V](avlStorage.get(StorageKey !@@ AvlTree.nodeKey(avlStorage.get(AvlTree.rootNodeKey).get)).get)
      val newrootNodes = rootNodes.dropWhile{ case (_, version) => !(version sameElements to)}
      val newRootNode = rootNodes.head._1
      logger.info(s"root node after rollback: ${newRootNode}")
      AvlTree[K, V](newRootNode, avlStorage, newrootNodes)
    }

  private def getRightPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      getRightPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: getRightPath(internalNode.rightChild)
    case _: EmptyNode[K, V] => List.empty
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      getLeftPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: getLeftPath(internalNode.leftChild)
    case _: EmptyNode[K, V] => List.empty
  }

  private def insert(newKey: K, newValue: V, node: Node[K, V])
                    (implicit kMonoid: Monoid[K],
                     kSer: Serializer[K],
                     vMonoid: Monoid[V],
                     vSer: Serializer[V]): Node[K, V] = node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        insert(newKey, newValue, restoredNode)
      case _: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
      case leafNode: LeafNode[K, V] =>
        if (leafNode.key === newKey) leafNode.copy(value = newValue)
        else {
          val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value, height = 1, balance = 0)
          val newNode = insert(
            newKey,
            newValue,
            newInternalNode
          )
          addToStat(newNode, leafNode)
          newNode
        }
      case internalNode: InternalNode[K, V] =>
        if (internalNode.key > newKey) {
          val newLeftChild = insert(newKey, newValue, internalNode.leftChild)
          val newNode = internalNode.updateChilds(newLeftChild = newLeftChild)
          addToStat(inserted = List(newNode, newLeftChild), deleted = internalNode :: Nil)
          balance(newNode)
        } else {
          val newRightChild = insert(newKey, newValue, internalNode.rightChild)
          val newNode = internalNode.updateChilds(newRightChild = newRightChild)
          addToStat(inserted = List(newNode, newRightChild), deleted = internalNode :: Nil)
          balance(newNode)
        }
    }

  private def balance(node: Node[K, V])
                     (implicit kMonoid: Monoid[K], kSer: Serializer[K], vMonoid: Monoid[V], vSer: Serializer[V]): Node[K, V] = {
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        balance(restoredNode)
      case internalNode: InternalNode[K, V] =>
        val newAdditionalInfo = (
          Math.abs(internalNode.balance),
          (internalNode.leftChild.height - internalNode.rightChild.height == 2) &&
            rightSubTreeHeight(internalNode.leftChild) > leftSubTreeHeight(internalNode.leftChild), //lr
          (internalNode.leftChild.height - internalNode.rightChild.height == 2) &&
            rightSubTreeHeight(internalNode.leftChild) <= leftSubTreeHeight(internalNode.leftChild), //r
          (internalNode.rightChild.height - internalNode.leftChild.height == 2) &&
            leftSubTreeHeight(internalNode.rightChild) > rightSubTreeHeight(internalNode.rightChild)
        )
        newAdditionalInfo match {
          case (_, true, _, _) =>
            if (loggable) logger.info("Lr rotation.")
            lrRotation(internalNode)
          case (_, _, _, true) =>
            if (loggable) logger.info("Rl rotation.")
            rlRotation(internalNode)
          case (_, _, true, _) =>
            if (loggable) logger.info("right rotation")
            rightRotation(internalNode)
          case (2, _, _, _) =>
            if (loggable) logger.info("left rotation.")
            leftRotation(internalNode)
          case _ =>
            internalNode
        }
      case leafNode: LeafNode[K, V] => leafNode
    }
  }.selfInspection

  private def rightSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      rightSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.rightChild.height
    case _                                => -1
  }

  private def leftSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      leftSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.leftChild.height
    case _                                => -1
  }

  private def rightRotation(node: Node[K, V])
                           (implicit kMonoid: Monoid[K],
                            kSer: Serializer[K],
                            vMonoid: Monoid[V],
                            vSer: Serializer[V]): Node[K, V] = {
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        rightRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val newRoot = internalNode.leftChild match {
          case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
          case internalNode: InternalNode[K, V] => internalNode
          case shadowNode: ShadowNode[K, V] =>
            shadowNode.restoreFullNode(avlStorage) match {
              case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
              case internalNode: InternalNode[K, V] => internalNode
            }
        }
        val newLeftChildForPrevRoot = newRoot.rightChild.selfInspection
        val prevRoot = internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot)
        val newUpdatedRoot =
          newRoot.updateChilds(newRightChild = prevRoot)
        val newNodes = List(newUpdatedRoot, prevRoot)
        val deletedNodes = List(internalNode, internalNode.leftChild)
        addToStat(newNodes, deletedNodes)
        newUpdatedRoot
    }
  }.selfInspection

  private def leftRotation(node: Node[K, V])
                          (implicit kMonoid: Monoid[K],
                           kSer: Serializer[K],
                           vMonoid: Monoid[V],
                           vSer: Serializer[V]): Node[K, V] = {
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        leftRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val newRoot = internalNode.rightChild match {
          case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
          case internalNode: InternalNode[K, V] => internalNode
          case shadowNode: ShadowNode[K, V] =>
            shadowNode.restoreFullNode(avlStorage) match {
              case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
              case internalNode: InternalNode[K, V] => internalNode
            }
        }
        val newRightChildForPrevRoot = newRoot.leftChild.selfInspection
        val prevRoot       = internalNode.updateChilds(newRightChild = newRightChildForPrevRoot)
        val newUpdatedRoot = newRoot.updateChilds(newLeftChild = prevRoot)
        val newNodes = List(newUpdatedRoot, prevRoot)
        val deletedNodes = List(internalNode, internalNode.rightChild)
        addToStat(newNodes, deletedNodes)
        newUpdatedRoot
    }
  }.selfInspection

  private def rlRotation(node: Node[K, V])
                        (implicit kMonoid: Monoid[K],
                         kSer: Serializer[K],
                         vMonoid: Monoid[V],
                         vSer: Serializer[V]): Node[K, V] = {
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        rlRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val rotatedRightChild = rightRotation(internalNode.rightChild)
        val updatedNode =
          internalNode.updateChilds(newRightChild = rotatedRightChild)
        val newNodes = List(updatedNode)
        val deletedNodes = List(internalNode, internalNode.rightChild)
        addToStat(newNodes, deletedNodes)
        leftRotation(updatedNode)
    }
  }.selfInspection

  private def lrRotation(node: Node[K, V])
                        (implicit kMonoid: Monoid[K],
                         kSer: Serializer[K],
                         vMonoid: Monoid[V],
                         vSer: Serializer[V]): Node[K, V] = {
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        lrRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val rotatedLeftChild = leftRotation(internalNode.leftChild)
        val updatedNode =
          internalNode.updateChilds(newLeftChild = rotatedLeftChild)
        val newNodes = List(updatedNode)
        val deletedNodes = List(internalNode, internalNode.leftChild)
        addToStat(newNodes, deletedNodes)
        rightRotation(updatedNode)
    }
  }.selfInspection

  override def close(): Unit = avlStorage.close()

  override def toString: String = rootNode.toString
}

object AvlTree {

  def restore[K: Monoid: Serializer: Hashable: Order, V: Monoid: Serializer](
    avlStorage: VersionalStorage,
    boxesStorage: VersionalStorage
  ): Try[AvlTree[K, V]] = Try {
    val rootNode = NodeSerilalizer.fromBytes[K, V](avlStorage.get(AvlTree.rootNodeKey).get)
    AvlTree(rootNode, avlStorage)
  }

  val rootNodeKey: StorageKey = StorageKey !@@ ((3: Byte) +: Algos.hash("root_node"))

  sealed trait Direction
  object Directions {
    case object LEFT  extends Direction
    case object RIGHT extends Direction
    case object EMPTY extends Direction
  }

  def apply[K: Monoid: Order: Hashable : Serializer, V: Monoid : Serializer](avlStorage: VersionalStorage): AvlTree[K, V] =
    new AvlTree[K, V](EmptyNode(), avlStorage)

  def elementKey(key: Array[Byte]): Array[Byte] = (0: Byte) +: key

  def nodeKey(key: Array[Byte]): Array[Byte] = (1: Byte) +: key

  def getChunks(node: Node[StorageKey, StorageValue],
                currentChunkHeight: Int,
                avlStorage: VersionalStorage)
               (implicit kSer: Serializer[StorageKey],
                vSer: Serializer[StorageValue],
                kM: Monoid[StorageKey],
                vM: Monoid[StorageValue],
                hashKey: Hashable[StorageKey]): List[SnapshotChunk] = {

    def restoreNodesUntilDepthAndReturnLeafs(depth: Int,
                                             node: Node[StorageKey, StorageValue]): (Node[StorageKey, StorageValue], List[Node[StorageKey, StorageValue]]) = node match {
      case shadowNode: ShadowNode[StorageKey, StorageValue] =>
        val newNode = shadowNode.restoreFullNode(avlStorage)
        restoreNodesUntilDepthAndReturnLeafs(depth, newNode)
      case internalNode: InternalNode[StorageKey, StorageValue] if depth != 0 =>
        val (recoveredLeftChild, leftSubTreeChildren) =
          restoreNodesUntilDepthAndReturnLeafs(depth - 1, internalNode.leftChild)
        val (recoveredRightChild, rightSubTreeChildren) =
          restoreNodesUntilDepthAndReturnLeafs(depth - 1, internalNode.rightChild)
        internalNode.copy(
          leftChild = recoveredLeftChild,
          rightChild = recoveredRightChild
        ) -> (rightSubTreeChildren ++ leftSubTreeChildren)
      case internalNode: InternalNode[StorageKey, StorageValue] =>
        internalNode -> List(internalNode.leftChild, internalNode.rightChild)
      case leaf: LeafNode[StorageKey, StorageValue] => leaf -> List.empty[Node[StorageKey, StorageValue]]
      case emptyNode: EmptyNode[StorageKey, StorageValue] => emptyNode -> List.empty
    }

    val (rootChunk: Node[StorageKey, StorageValue], rootChunkChildren) = restoreNodesUntilDepthAndReturnLeafs(currentChunkHeight, node)
    SnapshotChunk(rootChunk, ChunkId @@ rootChunk.hash) ::
      rootChunkChildren.flatMap(node => getChunks(node, currentChunkHeight, avlStorage))
  }
}
