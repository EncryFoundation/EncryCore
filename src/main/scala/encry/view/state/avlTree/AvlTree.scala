package encry.view.state.avlTree

import cats.syntax.order._
import cats.{Monoid, Order}
import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
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

final case class AvlTree[K : Hashable : Order, V] (rootNode: Node[K, V],
                                                   avlStorage: VersionalStorage) extends AutoCloseable with StrictLogging {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]] {
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  def root: K = rootNode.key

  val rootHash: Array[Byte] = rootNode.hash

  def insertAndDeleteMany(version: StorageVersion,
                          toInsert: List[(K, V)],
                          toDelete: List[K],
                          stateHeight: Height = Height @@ 0)
                         (implicit kSer: Serializer[K],
                         vSer: Serializer[V],
                         kM: Monoid[K],
                         vM: Monoid[V]): AvlTree[K, V] = {
    //toDeleteNodes = HashSet.empty
    val deleteStartTime = System.currentTimeMillis()
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDeleteKey) =>
        deleteKey(toDeleteKey, prevRoot)
    }
    logger.info(s"avl delete time: ${(System.currentTimeMillis() - deleteStartTime)/1000L}")
    val insertStartTime = System.currentTimeMillis()
    val newRoot = toInsert.foldLeft(rootAfterDelete) {
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        val res = insert(keyToInsert, valueToInsert, prevRoot)
        res
    }
    //logger.info(s"new root: $newRoot")
    logger.info(s"avl insert time: ${(System.currentTimeMillis() - insertStartTime)/1000L}")
    val notChangedKeysStart = System.currentTimeMillis()
    //val notChangedKeys = getNewNodesWithFirstUnchanged(newRoot).toSet
    logger.info(s"avl notChangedKeysStart: ${(System.currentTimeMillis() - notChangedKeysStart)/1000L}")
    val flattenNewNodesStart = System.currentTimeMillis()
    val (insertedNodesInTree, notChanged) = getNewNodesWithFirstUnchanged(newRoot)
    val insertedNodes   = insertedNodesInTree.map(node => node.hash -> node)
    //logger.info(s"Inserted nodes: ${insertedNodes.map(node => Algos.encode(node._2.hash)).mkString("\n")}")
    val notChangedKeys  = notChanged.map{node => ByteArrayWrapper(node.hash)}.toSet
    //logger.info(s"Not changed: ${notChanged.map(node => Algos.encode(node.hash)).mkString("\n")}")
    val deletedNodes    = takeUntil(rootNode, node => !notChangedKeys.contains(ByteArrayWrapper(node.hash)))
    //logger.info(s"Deleted nodes: ${deletedNodes.map(_.toString).mkString("\n")}")
    val startInsertTime = System.currentTimeMillis()
    val shadowedRoot    = ShadowNode.childsToShadowNode(newRoot)
    avlStorage.insert(
      version,
      toInsert.map {
        case (key, value) =>
          StorageKey @@ AvlTree.elementKey(kSer.toBytes(key)) -> StorageValue @@ vSer.toBytes(value)
      } ++
        insertedNodes.map {
          case (key, node) =>
            StorageKey @@ AvlTree.nodeKey(key) -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(node))
        }.toList ++
        List(AvlTree.rootNodeKey -> StorageValue @@ shadowedRoot.hash,
          UtxoState.bestHeightKey -> StorageValue @@ Ints.toByteArray(stateHeight)),
      deletedNodes.map(key => StorageKey @@ AvlTree.nodeKey(key.hash)) ++ toDelete.map(key =>
        StorageKey @@ AvlTree.elementKey(kSer.toBytes(key))
      )
    )
    logger.debug(s"time of insert in db: ${(System.currentTimeMillis() - startInsertTime)/1000L} s")
    AvlTree(shadowedRoot, avlStorage)
  }

  private def getNewNodesWithFirstUnchanged(node: Node[K, V]): (List[Node[K, V]], List[Node[K, V]]) = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restored = shadowNode.restoreFullNode(avlStorage)
      getNewNodesWithFirstUnchanged(restored)
    case internal: InternalNode[K, V] =>
      avlStorage.get(StorageKey @@ AvlTree.nodeKey(internal.hash)) match {
        case Some(_) => List.empty[Node[K, V]] -> (internal :: Nil)
        case None =>
          val leftScan = getNewNodesWithFirstUnchanged(internal.leftChild)
          val rightScan = getNewNodesWithFirstUnchanged(internal.rightChild)
          (internal :: leftScan._1 ::: rightScan._1) -> (leftScan._2 ::: rightScan._2)
      }
    case leafNode: LeafNode[K, V] =>
      avlStorage.get(StorageKey @@ AvlTree.nodeKey(leafNode.hash)) match {
        case Some(_) => List.empty[Node[K, V]] -> (leafNode :: Nil)
        case None => List(leafNode) -> List.empty
      }
    case emptyNode: EmptyNode[K, V] => List.empty[Node[K, V]] -> List.empty[Node[K, V]]
  }

  private def takeUntil(node: Node[K, V], predicate: Node[K, V] => Boolean): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] if predicate(shadowNode) =>
      val restored = shadowNode.restoreFullNode(avlStorage)
      takeUntil(restored, predicate)
    case internalNode: InternalNode[K, V] if predicate(internalNode) =>
      internalNode :: takeUntil(internalNode.leftChild, predicate) ::: takeUntil(internalNode.rightChild, predicate)
    case leafNode: LeafNode[K, V] if predicate(leafNode) =>
      List(leafNode)
    //case emptyNode: EmptyNode[K, V] => List(emptyNode)
    case _ => List.empty
  }


  private def flattenNewNodes(node: Node[K, V])(implicit kSer: Serializer[K],
                                                vSer: Serializer[V],
                                                kM: Monoid[K],
                                                vM: Monoid[V]): List[(StorageKey, StorageValue)] = node match {
    case internalNode: InternalNode[K, V] =>
      val internal =
        List(StorageKey @@ internalNode.hash -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(internalNode)))
      val left = flattenNewNodes(internalNode.leftChild)
      val right = flattenNewNodes(internalNode.rightChild)
      internal ++ left ++ right
    case leafNode: LeafNode[K, V] =>
      List(StorageKey @@ leafNode.hash -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(leafNode)))
    case _ => List.empty
  }

  private def toDeleteKeys(node: Node[K, V], predicate: Node[K, V] => Boolean): List[StorageKey] = node match {
    case shadowNode: ShadowNode[K, V] if predicate(shadowNode) =>
      val restored = shadowNode.restoreFullNode(avlStorage)
      toDeleteKeys(restored, predicate)
    case internalNode: InternalNode[K, V] if predicate(internalNode) =>
      (StorageKey @@ internalNode.hash) :: toDeleteKeys(internalNode.leftChild, predicate) :::
        toDeleteKeys(internalNode.rightChild, predicate)
    case leafNode: LeafNode[K, V] if predicate(leafNode) =>
      List(StorageKey @@ leafNode.hash)
    case emptyNode: EmptyNode[K, V] => List(StorageKey @@ emptyNode.hash)
    case _ => List.empty
  }

  def getOperationsRootHash(
    toInsert: List[(K, V)],
    toDelete: List[K],
  )(implicit kSer: Serializer[K], vSer: Serializer[V], kM: Monoid[K], vM: Monoid[V]): Try[Array[Byte]] = Try {
    //logger.info(s"root node in getOperationsRootHash: ${rootNode}")
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDelete) =>
        deleteKey(toDelete, prevRoot)
    }
    //logger.info(s"After deleting rootNode: ${rootAfterDelete}")
    val newRoot = toInsert.foldLeft(rootAfterDelete) {
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        insert(keyToInsert, valueToInsert, prevRoot)
    }
    //logger.info(s"new root should be: ${newRoot}")
    newRoot.hash
  }

  def get(k: K)(implicit kSer: Serializer[K], vSer: Serializer[V]): Option[V] =
    avlStorage.get(StorageKey !@@ AvlTree.elementKey(kSer.toBytes(k))).map(vSer.fromBytes)

  def contains(k: K)(implicit kSer: Serializer[K]): Boolean =
    avlStorage.get(StorageKey !@@ AvlTree.elementKey(kSer.toBytes(k))).isDefined

  def getInTree(k: K)(implicit kSer: Serializer[K]): Option[V] = getK(k, rootNode)

  def containsInTree(k: K)(implicit kSer: Serializer[K]): Boolean = find(k).isDefined

  def find(k: K)(implicit kSer: Serializer[K]): Option[(K, V)] = getK(k, rootNode).map { value =>
    (k, value)
  }

  private def getK(key: K, node: Node[K, V]): Option[V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      getK(key, restoredNode)
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key === key) Some(internalNode.value)
      else if (internalNode.key > key) getK(key, internalNode.leftChild)
      else getK(key, internalNode.rightChild)
    case leafNode: LeafNode[K, V] => if (leafNode.key === key) Some(leafNode.value) else None
  }

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
        //logger.info(s"DELETE1: add ${Algos.encode(leafNode.hash)}")
        EmptyNode[K, V]
      }
      else leafNode
    case internalNode: InternalNode[K, V] =>
      //logger.info(s"DELETE2: add ${Algos.encode(internalNode.hash)}")
      if (internalNode.key > key) {
        val newLeftChild = delete(internalNode.leftChild, key)
        val childUpdated = internalNode.updateChilds(newLeftChild = newLeftChild)
        val newNode      = childUpdated.selfInspection
        val balancedRoot = balance(newNode)
        balancedRoot
      } else if (internalNode.key < key) {
        val newRightChild = delete(internalNode.rightChild, key)
        val childUpdated = internalNode.updateChilds(newRightChild = newRightChild)
        val newNode      = childUpdated.selfInspection
        val balancedRoot = balance(newNode)
        balancedRoot
      } else {
        val theClosestValue = findTheClosestValue(internalNode, internalNode.key)
        //logger.info(s"theClosestValue for node ${internalNode} is ${theClosestValue._1._1}")
        val newNode = theClosestValue match {
          case ((newKey, newValue), LEFT) =>
            val newLeftChild = delete(internalNode.leftChild, newKey)
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newLeftChild = newLeftChild)
              .selfInspection
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = delete(internalNode.rightChild, newKey)
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newRightChild = newRightChild)
              .selfInspection
            newNode
          case ((_, _), EMPTY) => internalNode
        }
        balance(newNode)
        //logger.info(s"after deleting: ${balancedNode}")
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

  //[K: Monoid : Serializer : Hashable : Order, V: Monoid : Serializer]
  def rollbackTo(to: StorageVersion)
                (implicit kMonoid: Monoid[K], kSer: Serializer[K], vMonoid: Monoid[V], vSer: Serializer[V]): Try[AvlTree[K, V]] =
    Try {
      logger.debug(s"Rollback avl to version: ${Algos.encode(to)}")
      logger.debug(s"Versions in storage: ${avlStorage.versions.map(Algos.encode).mkString(",")}")
      logger.debug(s"Before rollback node key: ${Algos.encode(avlStorage.get(AvlTree.rootNodeKey).get)}")
      logger.debug(s"Before rollback root node: ${rootNode}")
      avlStorage.rollbackTo(to)
      logger.debug(s"Storage success rolled back")
      logger.debug(s"rootNodeKey: ${Algos.encode(avlStorage.get(AvlTree.rootNodeKey).get)}")
      val newRootNode =
        NodeSerilalizer.fromBytes[K, V](avlStorage.get(StorageKey !@@ AvlTree.nodeKey(avlStorage.get(AvlTree.rootNodeKey).get)).get)
      logger.debug(s"root node after rollback: ${newRootNode}")
      AvlTree[K, V](newRootNode, avlStorage)
    }

  private def getRightPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      getRightPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: getRightPath(internalNode.rightChild)
    case emptyNode: EmptyNode[K, V] => List.empty
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      getLeftPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: getLeftPath(internalNode.leftChild)
    case emptyNode: EmptyNode[K, V] => List.empty
  }

  private def insert(newKey: K, newValue: V, node: Node[K, V])
                    (implicit kMonoid: Monoid[K],
                     kSer: Serializer[K],
                     vMonoid: Monoid[V],
                     vSer: Serializer[V]): Node[K, V] =
    {
      node match {
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
            newNode
          }
        case internalNode: InternalNode[K, V] =>
          if (internalNode.key > newKey) {
            val newLeftChild = insert(newKey, newValue, internalNode.leftChild)
            val newNode = internalNode.updateChilds(newLeftChild = newLeftChild)
            balance(newNode)
          } else {
            val newRightChild = insert(newKey, newValue, internalNode.rightChild)
            val newNode = internalNode.updateChilds(newRightChild = newRightChild)
            balance(newNode)
          }
      }
    }

  private def balance(node: Node[K, V])
                     (implicit kMonoid: Monoid[K], kSer: Serializer[K], vMonoid: Monoid[V], vSer: Serializer[V]): Node[K, V] =
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
            lrRotation(internalNode)
          case (_, _, _, true) =>
            rlRotation(internalNode)
          case (_, _, true, _) =>
            rightRotation(internalNode)
          case (2, _, _, _) =>
            //println(s"left rotation! ${internalNode}")
            leftRotation(internalNode)
          case _ => internalNode
        }
      case leafNode: LeafNode[K, V] => leafNode
    }

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
                            vSer: Serializer[V]): Node[K, V] = node match {
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
      val prevRootWithUpdatedChildren =
        internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot)
      val prevRoot = prevRootWithUpdatedChildren.selfInspection
      val newUpdatedRoot =
        newRoot.updateChilds(newRightChild = prevRoot)
//      logger.info(s"RIGHT ROTATION: (${Algos.encode(newUpdatedRoot.hash)}) (${Algos.encode(internalNode.hash)}, " +
//        s"${internalNode.leftChild.map(el => Algos.encode(el.hash))})")
      newUpdatedRoot
  }

  private def leftRotation(node: Node[K, V])
                          (implicit kMonoid: Monoid[K],
                           kSer: Serializer[K],
                           vMonoid: Monoid[V],
                           vSer: Serializer[V]): Node[K, V] = node match {
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
      val prevRootWithUpdatedChildren =
        internalNode.updateChilds(newRightChild = newRightChildForPrevRoot)
      val prevRoot       = prevRootWithUpdatedChildren.selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newLeftChild = prevRoot)
//      logger.info(s"LEFT ROTATION: (${Algos.encode(newUpdatedRoot.hash)}) (${Algos.encode(internalNode.hash)}, " +
//        s"${internalNode.rightChild.map(el => Algos.encode(el.hash))})")
      newUpdatedRoot
  }

  private def rlRotation(node: Node[K, V])
                        (implicit kMonoid: Monoid[K],
                         kSer: Serializer[K],
                         vMonoid: Monoid[V],
                         vSer: Serializer[V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      rlRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedRightChild = rightRotation(internalNode.rightChild)
      val updatedNode =
        internalNode.updateChilds(newRightChild = rotatedRightChild)
      //logger.info(s"RL ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
      leftRotation(updatedNode)
  }

  private def lrRotation(node: Node[K, V])
                        (implicit kMonoid: Monoid[K],
                         kSer: Serializer[K],
                         vMonoid: Monoid[V],
                         vSer: Serializer[V]): Node[K, V] = node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        lrRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val rotatedLeftChild = leftRotation(internalNode.leftChild)
        val updatedNode =
          internalNode.updateChilds(newLeftChild = rotatedLeftChild)
        //logger.info(s"LR ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
        rightRotation(updatedNode)
    }

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
    import cats.implicits._

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
