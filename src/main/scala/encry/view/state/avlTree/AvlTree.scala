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
        val leftScan = internal.leftChild.map(getNewNodesWithFirstUnchanged).getOrElse(List.empty)
        val rightScan = internal.rightChild.map(getNewNodesWithFirstUnchanged).getOrElse(List.empty)
        leftScan ::: rightScan
      }
    case leafNode: LeafNode[K, V] =>
      if (storage.contains(StorageKey @@ leafNode.hash)) ByteArrayWrapper(leafNode.hash) :: Nil
      else List.empty
  }

  private def flattenNewNodes(node: Node[K, V])(implicit kSer: Serializer[K],
                                                vSer: Serializer[V],
                                                kM: Monoid[K],
                                                vM: Monoid[V]): List[(StorageKey, StorageValue)] = node match {
    case internalNode: InternalNode[K, V] =>
      val internal =
        List(StorageKey @@ internalNode.hash -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(internalNode)))
      val left = internalNode.leftChild.map(flattenNewNodes).getOrElse(List.empty)
      val right = internalNode.rightChild.map(flattenNewNodes).getOrElse(List.empty)
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
      (StorageKey @@ internalNode.hash) :: internalNode.leftChild.map(toDeleteKeys(_, predicate)).getOrElse(List.empty) :::
        internalNode.rightChild.map(toDeleteKeys(_, predicate)).getOrElse(List.empty)
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
      else if (internalNode.key > key) internalNode.leftChild.flatMap(child => getK(key, child))
      else internalNode.rightChild.flatMap(child => getK(key, child))
    case leafNode: LeafNode[K, V] => if (leafNode.key === key) Some(leafNode.value) else None
  }

  def deleteKey(key: K, node: Node[K, V])(implicit m: Monoid[K],
                                          v: Monoid[V],
                                          kSer: Serializer[K],
                                          vSer: Serializer[V]): Node[K, V] = {
    val delResult = delete(node, key)
    if (delResult.isEmpty) EmptyNode[K, V]()
    else delResult.get
  }

  private def delete(node: Node[K, V], key: K)(
    implicit m: Monoid[K],
    v: Monoid[V],
    kSer: Serializer[K],
    vSer: Serializer[V]
  ): (Option[Node[K, V]]) = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
      delete(restoredNode, key)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) {
        //logger.info(s"DELETE1: add ${Algos.encode(leafNode.hash)}")
        addToStat(deletedNodeHash = StorageKey @@ leafNode.hash)
        None
      }
      else Some(leafNode)
    case internalNode: InternalNode[K, V] =>
      //logger.info(s"DELETE2: add ${Algos.encode(internalNode.hash)}")
      addToStat(deletedNodeHash = StorageKey @@ internalNode.hash)
      if (internalNode.key > key) {
        val newLeftChild = internalNode.leftChild
          .map(node => delete(node, key))
          .getOrElse((Option.empty[Node[K, V]]))
        val childUpdated = internalNode.updateChilds(newLeftChild = newLeftChild)
        val newNode      = childUpdated.selfInspection
        val balancedRoot = balance(newNode)
        addToStat(
          newNodeHash = StorageKey @@ balancedRoot.hash,
          deletedNodeHash = StorageKey @@ internalNode.leftChild.map(_.hash).getOrElse(Array.emptyByteArray)
        )
        addToStat(
          newNodeHash = StorageKey @@ newLeftChild.map(_.hash).getOrElse(Array.emptyByteArray),
          deletedNodeHash = StorageKey @@ internalNode.hash
        )
        Some(balancedRoot)
      } else if (internalNode.key < key) {
        val newRightChild = internalNode.rightChild
          .map(node => delete(node, key))
          .getOrElse(Option.empty[Node[K, V]])
        val childUpdated = internalNode.updateChilds(newRightChild = newRightChild)
        val newNode      = childUpdated.selfInspection
        val balancedRoot = balance(newNode)
        addToStat(
          newNodeHash = StorageKey @@ balancedRoot.hash,
          deletedNodeHash = StorageKey @@ internalNode.rightChild.map(_.hash).getOrElse(Array.emptyByteArray)
        )
        addToStat(
          newNodeHash = StorageKey @@ newRightChild.map(_.hash).getOrElse(Array.emptyByteArray),
          deletedNodeHash = StorageKey @@ internalNode.hash
        )
        Some(balancedRoot)
      } else {
        val theClosestValue = findTheClosestValue(internalNode, internalNode.key)
        //logger.info(s"theClosestValue for node ${internalNode} is ${theClosestValue._1._1}")
        val newNode = theClosestValue match {
          case ((newKey, newValue), LEFT) =>
            val newLeftChild = internalNode.leftChild
              .map(node => delete(node, newKey))
              .getOrElse(Option.empty[Node[K, V]])
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newLeftChild = newLeftChild)
              .selfInspection
            addToStat(
              newNodeHash = StorageKey @@ newNode.hash,
              deletedNodeHash = StorageKey @@ internalNode.leftChild.map(_.hash).getOrElse(Array.emptyByteArray)
            )
            addToStat(
              newNodeHash = StorageKey @@ newLeftChild.map(_.hash).getOrElse(Array.emptyByteArray),
              deletedNodeHash = StorageKey @@ internalNode.hash
            )
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = internalNode.rightChild
              .map(node => delete(node, newKey))
              .getOrElse(Option.empty[Node[K, V]])
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newRightChild = newRightChild)
              .selfInspection
            addToStat(
              newNodeHash = StorageKey @@ newNode.hash,
              deletedNodeHash = StorageKey @@ internalNode.rightChild.map(_.hash).getOrElse(Array.emptyByteArray)
            )
            addToStat(
              newNodeHash = StorageKey @@ newRightChild.map(_.hash).getOrElse(Array.emptyByteArray),
              deletedNodeHash = StorageKey @@ internalNode.hash
            )
            newNode
          case ((_, _), EMPTY) => internalNode
        }
        Some(balance(newNode))
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
        val onLeft = internalNode.leftChild.flatMap { node =>
          val rightPath = getRightPath(node)
          rightPath.headOption.map(
            head =>
              rightPath.foldLeft[Node[K, V]](head) {
                case (bestNode, nodeToCompr) => h.max(bestNode, nodeToCompr)
              } -> LEFT
          )
        }
        val res = onLeft.orElse {
          val onRight = internalNode.rightChild.flatMap { node =>
            val leftPath = getLeftPath(node)
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
      internalNode +: internalNode.rightChild.map(getRightPath).getOrElse(List.empty)
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
      getLeftPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: internalNode.leftChild.map(getLeftPath).getOrElse(List.empty)
  }

  private def insert(newKey: K, newValue: V, node: Node[K, V])
                    (implicit kMonoid: Monoid[K],
                     kSer: Serializer[K],
                     vMonoid: Monoid[V],
                     vSer: Serializer[V]): Node[K, V] =
    {
      //println(s"insert: $newKey")
      node match {
        case shadowNode: ShadowNode[K, V] =>
          val restoredNode = shadowNode.restoreFullNode(storage)
          insert(newKey, newValue, restoredNode)
        case _: EmptyNode[K, V] => LeafNode[K, V](newKey, newValue)
        case leafNode: LeafNode[K, V] =>
          if (leafNode.key === newKey) {
            //(s"INSERT2: add ${Algos.encode(leafNode.hash)}")
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
          //logger.info(s"INSERT3: add ${Algos.encode(internalNode.hash)}")
          addToStat(deletedNodeHash = StorageKey @@ internalNode.hash)
          if (internalNode.key > newKey) {
            val newLeftChild = internalNode.leftChild
              .map(previousLeftChild => insert(newKey, newValue, previousLeftChild))
              .getOrElse{ LeafNode(newKey, newValue) }
            val newNode =
              internalNode.updateChilds(newLeftChild = Some(newLeftChild))
            addToStat(newNodeHash = StorageKey @@ newNode.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
            addToStat(newNodeHash = StorageKey @@ newLeftChild.hash)
            balance(newNode)
          } else {
            val newRightChild = internalNode.rightChild
              .map(previousRightChild => insert(newKey, newValue, previousRightChild))
              .getOrElse {LeafNode(newKey, newValue)}
            val newNode =
              internalNode.updateChilds(newRightChild = Some(newRightChild))
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
          internalNode.leftChild.exists(
            leftChild =>
              (leftChild.height - internalNode.rightChild.map(_.height).getOrElse(-1) == 2) &&
                rightSubTreeHeight(leftChild) > leftSubTreeHeight(leftChild)
          ), //lr
          internalNode.leftChild.exists(
            leftChild =>
              (leftChild.height - internalNode.rightChild.map(_.height).getOrElse(-1) == 2) &&
                rightSubTreeHeight(leftChild) <= leftSubTreeHeight(leftChild)
          ), //r
          internalNode.rightChild.exists(
            rightChild =>
              ({
                rightChild.height - internalNode.leftChild.map(_.height).getOrElse(-1) == 2
              }) && {
                leftSubTreeHeight(rightChild) > rightSubTreeHeight(rightChild)
            }
          ) //rl
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
    case internalNode: InternalNode[K, V] => internalNode.rightChild.map(_.height).getOrElse(-1)
    case _                                => -1
  }

  private def leftSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val restoredNode = shadowNode.restoreFullNode(storage)
      leftSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.leftChild.map(_.height).getOrElse(-1)
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
      val newRoot = internalNode.leftChild.get match {
        case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
        case shadowNode: ShadowNode[K, V] =>
          shadowNode.restoreFullNode(storage) match {
            case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
            case internalNode: InternalNode[K, V] => internalNode
          }
      }
      val (newLeftChildForPrevRoot) = {
        newRoot.rightChild.map(child => child.selfInspection)
      }
      val prevRootWithUpdatedChildren =
        internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot)
      val prevRoot = prevRootWithUpdatedChildren.selfInspection
      val newUpdatedRoot =
        newRoot.updateChilds(newRightChild = Some(prevRoot))
//      logger.info(s"RIGHT ROTATION: (${Algos.encode(newUpdatedRoot.hash)}) (${Algos.encode(internalNode.hash)}, " +
//        s"${internalNode.leftChild.map(el => Algos.encode(el.hash))})")
      addToStat(newNodeHash = StorageKey @@ newUpdatedRoot.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
      addToStat(deletedNodeHash = StorageKey @@ internalNode.leftChild.map(_.hash).getOrElse(Array.emptyByteArray))
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
      val newRoot = internalNode.rightChild.get match {
        case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
        case internalNode: InternalNode[K, V] => internalNode
        case shadowNode: ShadowNode[K, V] =>
          shadowNode.restoreFullNode(storage) match {
            case LeafNode(key, value)             => InternalNode(key, value, 0, 0)
            case internalNode: InternalNode[K, V] => internalNode
          }
      }
      //println("newRoot:" + newRoot)
      val (newRightChildForPrevRoot) = {newRoot.leftChild.map(child => child.selfInspection)}
      val prevRootWithUpdatedChildren =
        internalNode.updateChilds(newRightChild = newRightChildForPrevRoot)
      val prevRoot       = prevRootWithUpdatedChildren.selfInspection
      val newUpdatedRoot = newRoot.updateChilds(newLeftChild = Some(prevRoot))
//      logger.info(s"LEFT ROTATION: (${Algos.encode(newUpdatedRoot.hash)}) (${Algos.encode(internalNode.hash)}, " +
//        s"${internalNode.rightChild.map(el => Algos.encode(el.hash))})")
      addToStat(newNodeHash = StorageKey @@ newUpdatedRoot.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
      addToStat(deletedNodeHash = StorageKey @@ internalNode.rightChild.map(_.hash).getOrElse(Array.emptyByteArray))
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
      val rotatedRightChild = rightRotation(internalNode.rightChild.get)
      val updatedNode =
        internalNode.updateChilds(newRightChild = Some(rotatedRightChild))
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
        val rotatedLeftChild = leftRotation(internalNode.leftChild.get)
        val updatedNode =
          internalNode.updateChilds(newLeftChild = Some(rotatedLeftChild))
        //logger.info(s"LR ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
        addToStat(newNodeHash = StorageKey @@ updatedNode.hash, deletedNodeHash = StorageKey @@ internalNode.hash)
        rightRotation(updatedNode)
    }

  def selfInspectionAfterFastSync(implicit kSer: Serializer[K]): Boolean = {
    @scala.annotation.tailrec
    def loop(nodesToProcess: List[Node[K, V]], keysToInspect: List[Array[Byte]]): List[Array[Byte]] =
      if (nodesToProcess.nonEmpty) nodesToProcess.head match {
        case _: EmptyNode[K, V] => List.empty
        case s: ShadowNode[K, V] =>
          val restoredNode: Node[K, V] = s.restoreFullNode(storage)
          val removedExcess: List[Node[K, V]] = nodesToProcess.drop(1)
          val newToProcess: List[Node[K, V]] = restoredNode :: removedExcess
          loop(newToProcess, keysToInspect)
        case i: InternalNode[K, V] =>
          val leftChild: Option[(List[Node[K, V]], Node[K, V])] = getLeftChild(i)
          val rightChild: Option[(List[Node[K, V]], Node[K, V])] = getRightChild(i)
          val (next: List[Node[K, V]], current: List[Array[Byte]]) = (leftChild :: rightChild :: Nil)
            .foldLeft(List.empty[Node[K, V]], List.empty[Array[Byte]]) {
              case ((nextNodes, thisNodes), Some((nextChildren, thisNode))) =>
                (nextChildren ::: nextNodes, Algos.hash(kSer.toBytes(thisNode.key).reverse) :: thisNode.hash :: thisNodes)
              case (result, _) => result
            }
          val updatedNodeToProcess: List[Node[K, V]] = nodesToProcess.drop(1)
          loop(updatedNodeToProcess ::: next, Algos.hash(kSer.toBytes(i.key).reverse) :: i.hash :: current ::: keysToInspect)
        case l: LeafNode[K, V] => loop(nodesToProcess.drop(1), Algos.hash(kSer.toBytes(l.key).reverse) :: l.hash :: keysToInspect)
      } else keysToInspect
    val keys: Set[ByteArrayWrapper] = loop(List(rootNode), List.empty).map(ByteArrayWrapper(_)).toSet
    val allKeysFromDB: Set[ByteArrayWrapper] = storage.getAllKeys(-1)
      .map(ByteArrayWrapper(_)).toSet - ByteArrayWrapper(UtxoState.bestHeightKey) - ByteArrayWrapper(AvlTree.rootNodeKey)
    logger.debug(s"${keys.map(l => Algos.encode(l.data))}")
    (allKeysFromDB -- keys).isEmpty
  }

  @scala.annotation.tailrec
  private def handleChild(child: Node[K, V]): (List[Node[K, V]], Node[K, V]) = child match {
    case s: ShadowNode[K, V] => handleChild(s.restoreFullNode(storage))
    case i: InternalNode[K, V] => List(i.rightChild, i.leftChild).flatten -> i
    case l: LeafNode[K, V] => List.empty -> l
  }

  private def getLeftChild(node: InternalNode[K, V]): Option[(List[Node[K, V]], Node[K, V])] = node.leftChild match {
    case Some(child) => Some(handleChild(child))
    case n@None => n
  }

  private def getRightChild(node: InternalNode[K, V]): Option[(List[Node[K, V]], Node[K, V])] = node.rightChild match {
    case Some(child) => Some(handleChild(child))
    case n@None => n
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
        val (recoveredLeftChild, leftSubTreeChildren) = internalNode.leftChild
          .map(restoreNodesUntilDepthAndReturnLeafs(depth - 1, _))
          .separate.map(_.getOrElse(List.empty))
        val (recoveredRightChild, rightSubTreeChildren) = internalNode.rightChild
          .map(restoreNodesUntilDepthAndReturnLeafs(depth - 1, _))
          .separate.map(_.getOrElse(List.empty))
        internalNode.copy(
          leftChild = recoveredLeftChild,
          rightChild = recoveredRightChild
        ) -> (rightSubTreeChildren ++ leftSubTreeChildren)
      case internalNode: InternalNode[StorageKey, StorageValue] =>
        internalNode -> List(internalNode.leftChild, internalNode.rightChild).flatten
      case leaf: LeafNode[StorageKey, StorageValue] => leaf -> List.empty[Node[StorageKey, StorageValue]]
    }

    val (rootChunk: Node[StorageKey, StorageValue], rootChunkChildren) = restoreNodesUntilDepthAndReturnLeafs(currentChunkHeight, node)
    SnapshotChunk(rootChunk, ChunkId @@ rootChunk.hash) ::
      rootChunkChildren.flatMap(node => getChunks(node, currentChunkHeight, storage))
  }
}
