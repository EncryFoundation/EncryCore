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

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Try

final case class AvlTree[K : Hashable : Order, V] (rootNode: Node[K, V], storage: VersionalStorage) extends AutoCloseable with StrictLogging {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]] {
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  var toDeleteNodes: HashSet[ByteArrayWrapper] = HashSet.empty[ByteArrayWrapper]

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
    toDeleteNodes = HashSet.empty
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
    logger.info(s"new root: $newRoot")
    logger.info(s"avl insert time: ${(System.currentTimeMillis() - insertStartTime)/1000L}")
    val notChangedKeysStart = System.currentTimeMillis()
    //val notChangedKeys = getNewNodesWithFirstUnchanged(newRoot).toSet
    logger.info(s"avl notChangedKeysStart: ${(System.currentTimeMillis() - notChangedKeysStart)/1000L}")
    val flattenNewNodesStart = System.currentTimeMillis()
    val insertedNodes   = flattenNewNodes(newRoot)
    logger.info(s"avl flattenNewNodes: ${(System.currentTimeMillis() - flattenNewNodesStart)/1000L}")
    val takeUntilTime = System.currentTimeMillis()
    //val deletedNodes    = toDeleteKeys(rootNode, node => !notChangedKeys.contains(ByteArrayWrapper(node.hash)))
    logger.info(s"avl deletedNoedes: ${(System.currentTimeMillis() - takeUntilTime)/1000L}")
    val startInsertTime = System.currentTimeMillis()
    val shadowedRoot    = ShadowNode.childsToShadowNode(newRoot)
    val insertedKeys = insertedNodes.map(_._1)
    //logger.info(s"insertedNodes: ${insertedNodes.map{ case (key, _) => Algos.encode(key)}}")
    storage.insert(
      version,
      toInsert.map {
        case (key, value) =>
          //logger.info(s"insert key: ${Algos.encode(kSer.toBytes(key))}")
          StorageKey @@ kSer.toBytes(key) -> StorageValue @@ vSer.toBytes(value)
      } ++ insertedNodes ++
        List(AvlTree.rootNodeKey -> StorageValue @@ shadowedRoot.hash,
          UtxoState.bestHeightKey -> StorageValue @@ Ints.toByteArray(stateHeight)),
      getDeletedNodesHashes(insertedKeys) ++ toDelete.map(key => {
        //logger.info(s"Delete key: ${Algos.encode(kSer.toBytes(key))}")
        StorageKey @@ kSer.toBytes(key)
      })
    )
    logger.info(s"avl insertion time: ${(System.currentTimeMillis() - startInsertTime)/1000L}")
    toDeleteNodes = HashSet.empty
    AvlTree(shadowedRoot, storage)
  }

  def getDeletedNodesHashes(inserted: List[StorageKey]): List[StorageKey] = {
    val insertedHashSet: Set[ByteArrayWrapper] = inserted.map(ByteArrayWrapper.apply).toSet
    val toDelete = toDeleteNodes.collect {
      case hash if storage.contains(StorageKey @@ hash.data) && !insertedHashSet.contains(hash) => StorageKey @@ hash.data
    }.toList
    toDeleteNodes = HashSet.empty
    toDelete
  }

  private def getNewNodesWithFirstUnchanged(node: Node[K, V]): List[ByteArrayWrapper] = node match {
    case shadowNode: ShadowNode[K, V] =>
      shadowNode.tryRestore(storage).map(getNewNodesWithFirstUnchanged).getOrElse(
        List(ByteArrayWrapper(shadowNode.hash))
      )
    case internal: InternalNode[K, V] =>
      if (storage.contains(StorageKey @@ internal.hash)) {
        List(ByteArrayWrapper(internal.hash))
      } else {
        val leftScan = getNewNodesWithFirstUnchanged(internal.leftChild)
        val rightScan = getNewNodesWithFirstUnchanged(internal.rightChild)
        leftScan ::: rightScan
      }
    case leafNode: LeafNode[K, V] =>
      if (storage.contains(StorageKey @@ leafNode.hash)) ByteArrayWrapper(leafNode.hash) :: Nil
      else List.empty
    case emptyNode: EmptyNode[K, V] => List.empty
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
      val restored = shadowNode.restoreFullNode(storage)
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
    toDeleteNodes = HashSet.empty
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDelete) =>
        deleteKey(toDelete, prevRoot)
    }
    //logger.info(s"After deleting rootNode: ${rootAfterDelete}")
    val newRoot = toInsert.foldLeft(rootAfterDelete) {
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        insert(keyToInsert, valueToInsert, prevRoot)
    }
    toDeleteNodes = HashSet.empty
    //logger.info(s"new root should be: ${newRoot}")
    newRoot.hash
  }

  def get(k: K)(implicit kSer: Serializer[K], vSer: Serializer[V]): Option[V] =
    storage.get(StorageKey !@@ kSer.toBytes(k)).map(vSer.fromBytes)

  def contains(k: K)(implicit kSer: Serializer[K]): Boolean =
    storage.get(StorageKey !@@ kSer.toBytes(k)).isDefined

  def getInTree(k: K)(implicit kSer: Serializer[K]): Option[V] = getK(k, rootNode)

  def containsInTree(k: K)(implicit kSer: Serializer[K]): Boolean = find(k).isDefined

  def find(k: K)(implicit kSer: Serializer[K]): Option[(K, V)] = getK(k, rootNode).map { value =>
    (k, value)
  }

  private def getK(key: K, node: Node[K, V]): Option[V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
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
      val restoredNode = shadowNode.restoreFullNode(storage)
      delete(restoredNode, key)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) {
        //logger.info(s"DELETE1: add ${Algos.encode(leafNode.hash)}")
        addToStat(deletedNodeHash = StorageKey @@ leafNode.hash)
        EmptyNode[K, V]
      }
      else leafNode
    case internalNode: InternalNode[K, V] =>
      //logger.info(s"DELETE2: add ${Algos.encode(internalNode.hash)}")
      addToStat(deletedNodeHash = StorageKey @@ internalNode.hash)
      if (internalNode.key > key) {
        val newLeftChild = delete(internalNode.leftChild, key)
        val childUpdated = internalNode.updateChilds(newLeftChild = newLeftChild)
        val newNode      = childUpdated.selfInspection
        val balancedRoot = balance(newNode)
        addToStat(
          newNodeHash = StorageKey @@ balancedRoot.hash,
          deletedNodeHash = StorageKey @@ internalNode.leftChild.hash
        )
        addToStat(
          newNodeHash = StorageKey @@ newLeftChild.hash,
          deletedNodeHash = StorageKey @@ internalNode.hash
        )
        balancedRoot
      } else if (internalNode.key < key) {
        val newRightChild = delete(internalNode.rightChild, key)
        val childUpdated = internalNode.updateChilds(newRightChild = newRightChild)
        val newNode      = childUpdated.selfInspection
        val balancedRoot = balance(newNode)
        addToStat(
          newNodeHash = StorageKey @@ balancedRoot.hash,
          deletedNodeHash = StorageKey @@ internalNode.rightChild.hash
        )
        addToStat(
          newNodeHash = StorageKey @@ newRightChild.hash,
          deletedNodeHash = StorageKey @@ internalNode.hash
        )
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
            addToStat(
              newNodeHash = StorageKey @@ newNode.hash,
              deletedNodeHash = StorageKey @@ internalNode.leftChild.hash
            )
            addToStat(
              newNodeHash = StorageKey @@ newLeftChild.hash,
              deletedNodeHash = StorageKey @@ internalNode.hash
            )
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = delete(internalNode.rightChild, newKey)
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newRightChild = newRightChild)
              .selfInspection
            addToStat(
              newNodeHash = StorageKey @@ newNode.hash,
              deletedNodeHash = StorageKey @@ internalNode.rightChild.hash
            )
            addToStat(
              newNodeHash = StorageKey @@ newRightChild.hash,
              deletedNodeHash = StorageKey @@ internalNode.hash
            )
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
        val restoredNode = shadowNode.restoreFullNode(storage)
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
      logger.debug(s"Versions in storage: ${storage.versions.map(Algos.encode).mkString(",")}")
      logger.debug(s"Before rollback node key: ${Algos.encode(storage.get(AvlTree.rootNodeKey).get)}")
      logger.debug(s"Before rollback root node: ${rootNode}")
      storage.rollbackTo(to)
      logger.debug(s"Storage success rolled back")
      logger.debug(s"rootNodeKey: ${Algos.encode(storage.get(AvlTree.rootNodeKey).get)}")
      val newRootNode =
        NodeSerilalizer.fromBytes[K, V](storage.get(StorageKey !@@ storage.get(AvlTree.rootNodeKey).get).get)
      logger.debug(s"root node after rollback: ${newRootNode}")
      AvlTree[K, V](newRootNode, storage)
    }

  private def getRightPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
      getRightPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: getRightPath(internalNode.rightChild)
    case emptyNode: EmptyNode[K, V] => List.empty
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
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
          val restoredNode = shadowNode.restoreFullNode(storage)
          insert(newKey, newValue, restoredNode)
        case _: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
        case leafNode: LeafNode[K, V] =>
          if (leafNode.key === newKey) {
            addToStat(deletedNodeHash = StorageKey @@ leafNode.hash)
            leafNode.copy(value = newValue)
          }
          else {
            val newInternalNode = InternalNode[K, V](leafNode.key, leafNode.value, height = 1, balance = 0)
            val newNode = insert(
              newKey,
              newValue,
              newInternalNode
            )
            addToStat(newNodeHash = StorageKey @@ newInternalNode.hash)
            newNode
          }
        case internalNode: InternalNode[K, V] =>
          addToStat(deletedNodeHash = StorageKey @@ internalNode.hash)
          if (internalNode.key > newKey) {
            val newLeftChild = insert(newKey, newValue, internalNode.leftChild)
            val newNode = internalNode.updateChilds(newLeftChild = newLeftChild)
            addToStat(newNodeHash = StorageKey @@ newNode.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
            addToStat(newNodeHash = StorageKey @@ newLeftChild.hash)
            balance(newNode)
          } else {
            val newRightChild = insert(newKey, newValue, internalNode.rightChild)
            val newNode =
              internalNode.updateChilds(newRightChild = newRightChild)
            addToStat(newNodeHash = StorageKey @@ newNode.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
            addToStat(newNodeHash = StorageKey @@ newRightChild.hash)
            balance(newNode)
          }
      }
    }

  private def addToStat(newNodeHash: StorageKey = StorageKey @@ Array.emptyByteArray,
                        deletedNodeHash: StorageKey = StorageKey @@ Array.emptyByteArray) = {
    toDeleteNodes -= ByteArrayWrapper(deletedNodeHash)
    if (newNodeHash.nonEmpty) toDeleteNodes += ByteArrayWrapper(newNodeHash)
  }

  private def balance(node: Node[K, V])
                     (implicit kMonoid: Monoid[K], kSer: Serializer[K], vMonoid: Monoid[V], vSer: Serializer[V]): Node[K, V] =
    node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(storage)
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
      val restoredNode = shadowNode.restoreFullNode(storage)
      rightSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.rightChild.height
    case _                                => -1
  }

  private def leftSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
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
      val restoredNode = shadowNode.restoreFullNode(storage)
      rightRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.leftChild match {
        case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
        case shadowNode: ShadowNode[K, V] =>
          shadowNode.restoreFullNode(storage) match {
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
      addToStat(newNodeHash = StorageKey @@ newUpdatedRoot.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
      addToStat(deletedNodeHash = StorageKey @@ internalNode.leftChild.hash)
      newUpdatedRoot
  }

  private def leftRotation(node: Node[K, V])
                          (implicit kMonoid: Monoid[K],
                           kSer: Serializer[K],
                           vMonoid: Monoid[V],
                           vSer: Serializer[V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
      leftRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val newRoot = internalNode.rightChild match {
        case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
        case shadowNode: ShadowNode[K, V] =>
          shadowNode.restoreFullNode(storage) match {
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
      addToStat(newNodeHash = StorageKey @@ newUpdatedRoot.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
      addToStat(deletedNodeHash = StorageKey @@ internalNode.rightChild.hash)
      newUpdatedRoot
  }

  private def rlRotation(node: Node[K, V])
                        (implicit kMonoid: Monoid[K],
                         kSer: Serializer[K],
                         vMonoid: Monoid[V],
                         vSer: Serializer[V]): Node[K, V] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
      rlRotation(restoredNode)
    case leafNode: LeafNode[K, V] => leafNode
    case internalNode: InternalNode[K, V] =>
      val rotatedRightChild = rightRotation(internalNode.rightChild)
      val updatedNode =
        internalNode.updateChilds(newRightChild = rotatedRightChild)
      //logger.info(s"RL ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
      addToStat(newNodeHash = StorageKey @@ updatedNode.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
      leftRotation(updatedNode)
  }

  private def lrRotation(node: Node[K, V])
                        (implicit kMonoid: Monoid[K],
                         kSer: Serializer[K],
                         vMonoid: Monoid[V],
                         vSer: Serializer[V]): Node[K, V] = node match {
      case shadowNode: ShadowNode[K, V] =>
        val restoredNode = shadowNode.restoreFullNode(storage)
        lrRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val rotatedLeftChild = leftRotation(internalNode.leftChild)
        val updatedNode =
          internalNode.updateChilds(newLeftChild = rotatedLeftChild)
        //logger.info(s"LR ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
        addToStat(newNodeHash = StorageKey @@ updatedNode.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
        rightRotation(updatedNode)
    }

  override def close(): Unit = storage.close()

  override def toString: String = rootNode.toString
}

object AvlTree {

  def restore[K: Monoid: Serializer: Hashable: Order, V: Monoid: Serializer](
    storage: VersionalStorage
  ): Try[AvlTree[K, V]] = Try {
    val rootNode = NodeSerilalizer.fromBytes[K, V](storage.get(AvlTree.rootNodeKey).get)
    AvlTree(rootNode, storage)
  }

  val rootNodeKey: StorageKey = StorageKey !@@ Algos.hash("root_node")

  sealed trait Direction
  object Directions {
    case object LEFT  extends Direction
    case object RIGHT extends Direction
    case object EMPTY extends Direction
  }

  def apply[K: Monoid: Order: Hashable : Serializer, V: Monoid : Serializer](storage: VersionalStorage): AvlTree[K, V] =
    new AvlTree[K, V](EmptyNode(), storage)

  def getChunks(node: Node[StorageKey, StorageValue],
                currentChunkHeight: Int,
                storage: VersionalStorage)
               (implicit kSer: Serializer[StorageKey],
                vSer: Serializer[StorageValue],
                kM: Monoid[StorageKey],
                vM: Monoid[StorageValue],
                hashKey: Hashable[StorageKey]): List[SnapshotChunk] = {
    import cats.implicits._

    def restoreNodesUntilDepthAndReturnLeafs(depth: Int,
                                             node: Node[StorageKey, StorageValue]): (Node[StorageKey, StorageValue], List[Node[StorageKey, StorageValue]]) = node match {
      case shadowNode: ShadowNode[StorageKey, StorageValue] =>
        val newNode = shadowNode.restoreFullNode(storage)
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
      rootChunkChildren.flatMap(node => getChunks(node, currentChunkHeight, storage))
  }
}
