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
//import encry.EncryApp.{influxRef, settings}
import encry.stats.StatsSender.AvlStat
import scala.collection.immutable.{HashMap}

final case class AvlTree[K : Hashable : Order, V](rootNode: Node[K, V],
                                                  avlStorage: VersionalStorage) extends AutoCloseable with StrictLogging {

  implicit def nodeOrder(implicit ord: Order[K]): Order[Node[K, V]] = new Order[Node[K, V]] {
    override def compare(x: Node[K, V], y: Node[K, V]): Int = ord.compare(x.key, y.key)
  }

  def root: K = rootNode.key

  val rootHash: Array[Byte] = rootNode.hash

  var nodesBuffer: List[Node[K, V]] = List.empty
  var nodesInsertionStat: List[(ByteArrayWrapper, Int)] = List.empty
  var collectionsTime: Long = 0
  var bufferTime: Long = 0
  var balanceTime: Long = 0
  var restoringTime: Long = 0
  var updateChilds: Long = 0

  def insertAndDeleteMany(version: StorageVersion,
                          toInsert: List[(K, V)],
                          toDelete: List[K],
                          stateHeight: Height = Height @@ 0)
                         (implicit kSer: Serializer[K],
                         vSer: Serializer[V],
                         kM: Monoid[K],
                         vM: Monoid[V]): AvlTree[K, V] = {
    //toDeleteNodes = HashSet.empty
    val deleteStartTime = System.nanoTime()
    val rootAfterDelete = toDelete.foldLeft(rootNode) {
      case (prevRoot, toDeleteKey) =>
        val res = deleteKey(toDeleteKey, prevRoot)
        res
    }
    val avlDeleteTime = System.nanoTime() - deleteStartTime
    logger.info(s"Colls: ${(collectionsTime / 1000000)} ms")
    logger.info(s"Buffer: ${(bufferTime / 1000000)} ms")
    logger.info(s"Balance: ${(balanceTime / 1000000)} ms")
    logger.info(s"Restore: ${(restoringTime / 1000000)} ms")
    logger.info(s"Update childs: ${(updateChilds / 1000000)} ms")
    logger.info(s"avl delete time: ${avlDeleteTime / 1000000} ms. Elems qty: ${toDelete.length}")
    balanceTime = 0
    collectionsTime = 0
    bufferTime = 0
    updateChilds = 0
    restoringTime = 0
    val insertStartTime = System.nanoTime()
    val newRoot = toInsert.foldLeft(rootAfterDelete) {
      case (prevRoot, (keyToInsert, valueToInsert)) =>
        val res = insert(keyToInsert, valueToInsert, prevRoot)
        res
    }
    val insertTime = System.nanoTime() - insertStartTime
    logger.info(s"Colls: ${(collectionsTime / 1000000)} ms")
    logger.info(s"Balance: ${(balanceTime / 1000000)} ms")
    logger.info(s"Buffer: ${(bufferTime / 1000000)} ms")
    logger.info(s"Update childs: ${(updateChilds / 1000000)} ms")
    logger.info(s"Restore: ${(restoringTime / 1000000)} ms")
    //logger.info(s"new root: $newRoot")
    logger.info(s"avl insert time: ${(insertTime / 1000000)} ms. Elems qty: ${toInsert.length}")
    val shadowedRoot    = ShadowNode.childsToShadowNode(newRoot)
//    logger.info(s"INSERT ELEMS: ${toInsert.map(elem => Algos.encode(implicitly[Serializer[K]].toBytes(elem._1)))}")
//    logger.info(s"DELETE ELEMS: ${toDelete.map(elem => Algos.encode(implicitly[Serializer[K]].toBytes(elem)))}")
//    logger.info(s"INSERTED NODES: ${insertedNodesBuffer.keys.map(elem => Algos.encode(elem.data)).mkString(",")}")
//    logger.info(s"DELETED NODES: ${deletedNodesBuffer.map(elem => Algos.encode(elem.data)).mkString(",")}")
    var collectionsUpdate: Long = System.nanoTime()
    //logger.info(nodesInsertionStat.map {case (key, stat) => s"${Algos.encode(key.data) -> stat}"}.mkString(","))
    val insertedNodesMap = nodesBuffer.map(node => ByteArrayWrapper(node.hash) -> node).toMap
    var insertedNodes: Map[ByteArrayWrapper, Node[K, V]] = Map.empty
    var deletedNodes: List[ByteArrayWrapper] = List.empty
    val nodeInsertionMap = nodesInsertionStat.reverse.toMap
    //logger.info(s"Nodes buffer: ${nodeInsertionMap.map(elem => Algos.encode(elem._1.data) -> elem._2)}")
    nodesBuffer.foreach { node =>
      val wrappedHash = ByteArrayWrapper(node.hash)
      val stat = nodeInsertionMap(wrappedHash)
      if (stat >= 0 && wrappedHash.size != 0) {
        insertedNodes = insertedNodes + (wrappedHash -> node)
      }
      else if (wrappedHash.size != 0) deletedNodes = wrappedHash :: deletedNodes
    }
//    logger.info(s"Inserted nodes: ${insertedNodes.keys.map(elem => Algos.encode(elem.data)).mkString(",")}")
//    logger.info(s"Deleted node: ${deletedNodes.map(elem => Algos.encode(elem.data)).mkString(",")}")
//    val (insertedNodesStat, deletedNodesStat) =
//      nodesInsertionStat.foldLeft(List.empty[(ByteArrayWrapper, Node[K, V])], List.empty[ByteArrayWrapper]) {
//        case ((toInsert, toDelete), (nodeHash, stat)) =>
//          if (stat >= 0) ((nodeHash -> insertedNodesMap(nodeHash)) :: toInsert) -> toDelete
//          else toInsert -> (nodeHash :: toDelete)
//      }
    logger.info(s"Coll update: ${(System.nanoTime() - collectionsUpdate)/1000000} ms.")
//    logger.info(s"Delete nodes: ${deletedNodes.map(elem => Algos.encode(elem.data)).mkString(",")}")
//    logger.info(s"Inserted nodes: ${insertedNodes.map(elem => Algos.encode(elem._1.data)).mkString(",")}")
    val startInsertTime = System.nanoTime()
    var nodesToDelete: List[ByteArrayWrapper] = deletedNodes.toSet[ByteArrayWrapper].filterNot(insertedNodes.map(_._1).toSet).toList
//    logger.info(s"DIFF: ${nodesToDelete.length}")
//    logger.info(s"Insert nodes: ${insertedNodes.map {case (key, _) => Algos.encode(key.data)}.mkString(",")}")
//    logger.info(s"Deleted node: ${nodesToDelete.map(key => Algos.encode(key.data)).mkString(",")}")
    avlStorage.insert(
      version,
      toInsert.map {
        case (key, value) =>
          StorageKey @@ AvlTree.elementKey(kSer.toBytes(key)) -> StorageValue @@ vSer.toBytes(value)
      } ++
        insertedNodes.map {
          case (key, node) =>
            StorageKey @@ AvlTree.nodeKey(key.data) -> StorageValue @@ NodeSerilalizer.toBytes(ShadowNode.childsToShadowNode(node))
        }.toList ++
        List(AvlTree.rootNodeKey -> StorageValue @@ shadowedRoot.hash,
          UtxoState.bestHeightKey -> StorageValue @@ Ints.toByteArray(stateHeight)),
      nodesToDelete.distinct.collect {
        case key if avlStorage.contains(StorageKey @@ AvlTree.nodeKey(key.data)) =>
          StorageKey @@ AvlTree.nodeKey(key.data)
      } ++ toDelete.map(key =>
        StorageKey @@ AvlTree.elementKey(kSer.toBytes(key))
      )
    )
    val insertDbTime = (System.nanoTime() - startInsertTime)
    if (settings.influxDB.isDefined) influxRef.get ! AvlStat(
      0,
      0,
      insertDbTime,
      avlDeleteTime,
      insertTime
    )
    nodesBuffer = List.empty
    nodesInsertionStat = List.empty
    AvlTree(shadowedRoot, avlStorage)
  }

  private def getNewNodesWithFirstUnchanged(node: Node[K, V]): (List[Node[K, V]], Set[ByteArrayWrapper]) = node match {
    case shadowNode: ShadowNode[K, V] => List.empty[Node[K, V]] -> Set(ByteArrayWrapper(shadowNode.nodeHash))
    case internal: InternalNode[K, V] =>
      avlStorage.get(StorageKey @@ AvlTree.nodeKey(internal.hash)) match {
        case Some(_) => List.empty[Node[K, V]] -> Set(ByteArrayWrapper(internal.hash))
        case None =>
          val leftScan = getNewNodesWithFirstUnchanged(internal.leftChild)
          val rightScan = getNewNodesWithFirstUnchanged(internal.rightChild)
          (internal :: leftScan._1 ::: rightScan._1) -> (leftScan._2 ++ rightScan._2)
      }
    case leafNode: LeafNode[K, V] =>
      avlStorage.get(StorageKey @@ AvlTree.nodeKey(leafNode.hash)) match {
        case Some(_) => List.empty[Node[K, V]] -> Set(ByteArrayWrapper(leafNode.hash))
        case None => List(leafNode) -> Set.empty
      }
    case emptyNode: EmptyNode[K, V] => List.empty[Node[K, V]] -> Set.empty[ByteArrayWrapper]
  }

  private def takeUntil(node: Node[K, V], predicate: Node[K, V] => Boolean): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] if predicate(shadowNode) =>
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
      takeUntil(restoredNode, predicate)
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
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
      toDeleteKeys(restoredNode, predicate)
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
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
      getK(key, restoredNode)
    case internalNode: InternalNode[K, V] =>
      if (internalNode.key === key) Some(internalNode.value)
      else if (internalNode.key > key) getK(key, internalNode.leftChild)
      else getK(key, internalNode.rightChild)
    case leafNode: LeafNode[K, V] => if (leafNode.key === key) Some(leafNode.value) else None
  }

  def addToStat(inserted: List[Node[K, V]] = List.empty, deleted: List[Node[K, V]] = List.empty): Unit = {
//    logger.info("=======================================================================")
//    logger.info(s"Delete: ${deleted.map(node => Algos.encode(node.hash)).mkString(",")}")
//    logger.info(s"Insert: ${inserted.map(node => Algos.encode(node.hash)).mkString(",")}")
//    logger.info("=======================================================================")
    val startBuffetTime = System.nanoTime()
    nodesBuffer = inserted ::: nodesBuffer
    bufferTime = bufferTime + (System.nanoTime() - startBuffetTime)
    val startTime = System.nanoTime()
    nodesInsertionStat = deleted.map { node =>
      val wrappedNodeHash = ByteArrayWrapper(node.hash)
      wrappedNodeHash -> -1
    } ::: nodesInsertionStat
    nodesInsertionStat = inserted.map { node =>
      val wrappedNodeHash = ByteArrayWrapper(node.hash)
      wrappedNodeHash -> 1
    } ::: nodesInsertionStat
    collectionsTime = collectionsTime + (System.nanoTime() - startTime)
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
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
      delete(restoredNode, key)
    case leafNode: LeafNode[K, V] =>
      if (leafNode.key === key) {
        //logger.info(s"DELETE1: add ${Algos.encode(leafNode.hash)}")
        addToStat(deleted = leafNode :: Nil)
        EmptyNode[K, V]
      }
      else leafNode
    case internalNode: InternalNode[K, V] =>
      //logger.info(s"DELETE2: add ${Algos.encode(internalNode.hash)}")
      if (internalNode.key > key) {
        val newLeftChild = delete(internalNode.leftChild, key)
        var startTime = System.nanoTime()
        val childUpdated = internalNode.updateChilds(newLeftChild = newLeftChild)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
        val deletedNodes = List(internalNode.leftChild, internalNode)
        val newNodes     = List(newLeftChild, childUpdated)
        addToStat(newNodes, deletedNodes)
        balance(childUpdated)
      } else if (internalNode.key < key) {
        val newRightChild = delete(internalNode.rightChild, key)
        var startTime = System.nanoTime()
        val childUpdated = internalNode.updateChilds(newRightChild = newRightChild)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
        val newNode      = childUpdated.selfInspection
        val deletedNodes = List(internalNode.rightChild, internalNode)
        val newNodes     = List(newRightChild, childUpdated)
        addToStat(newNodes, deletedNodes)
        balance(newNode)
      } else {
        val theClosestValue = findTheClosestValue(internalNode, internalNode.key)
        //logger.info(s"theClosestValue for node ${internalNode} is ${theClosestValue._1._1}")
        val newNode = theClosestValue match {
          case ((newKey, newValue), LEFT) =>
            val newLeftChild = delete(internalNode.leftChild, newKey)
            var startTime = System.nanoTime()
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newLeftChild = newLeftChild)
              .selfInspection
            updateChilds = updateChilds + (System.nanoTime() - startTime)
            val deletedNodes = List(internalNode.leftChild, internalNode)
            val newNodes     = List(newNode, newLeftChild)
            addToStat(newNodes, deletedNodes)
            newNode
          case ((newKey, newValue), RIGHT) =>
            val newRightChild = delete(internalNode.rightChild, newKey)
            var startTime = System.nanoTime()
            val newNode = internalNode
              .copy(key = newKey, value = newValue)
              .updateChilds(newRightChild = newRightChild)
              .selfInspection
            updateChilds = updateChilds + (System.nanoTime() - startTime)
            val deletedNodes = List(internalNode.rightChild, internalNode)
            val newNodes     = List(newNode, newRightChild)
            addToStat(newNodes, deletedNodes)
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
        val startTime = System.nanoTime()
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        restoringTime = restoringTime + (System.nanoTime() - startTime)
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
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
      getRightPath(restoredNode)
    case leafNode: LeafNode[K, V] => List(leafNode)
    case internalNode: InternalNode[K, V] =>
      internalNode +: getRightPath(internalNode.rightChild)
    case emptyNode: EmptyNode[K, V] => List.empty
  }

  private def getLeftPath(node: Node[K, V]): List[Node[K, V]] = node match {
    case shadowNode: ShadowNode[K, V] =>
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
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
          val startTime = System.nanoTime()
          val restoredNode = shadowNode.restoreFullNode(avlStorage)
          restoringTime = restoringTime + (System.nanoTime() - startTime)
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
            var startTime = System.nanoTime()
            val newNode = internalNode.updateChilds(newLeftChild = newLeftChild)
            updateChilds = updateChilds + (System.nanoTime() - startTime)
            addToStat(inserted = List(newNode, newLeftChild), deleted = internalNode :: Nil)
            balance(newNode)
          } else {
            val newRightChild = insert(newKey, newValue, internalNode.rightChild)
            var startTime = System.nanoTime()
            val newNode = internalNode.updateChilds(newRightChild = newRightChild)
            updateChilds = updateChilds + (System.nanoTime() - startTime)
            addToStat(inserted = List(newNode, newRightChild), deleted = internalNode :: Nil)
            balance(newNode)
          }
      }
    }

  private def balance(node: Node[K, V])
                     (implicit kMonoid: Monoid[K], kSer: Serializer[K], vMonoid: Monoid[V], vSer: Serializer[V]): Node[K, V] = {
    val startTime = System.nanoTime()
    val res = node match {
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
    balanceTime = balanceTime + (System.nanoTime() - startTime)
    res
  }.selfInspection

  private def rightSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
      rightSubTreeHeight(restoredNode)
    case internalNode: InternalNode[K, V] => internalNode.rightChild.height
    case _                                => -1
  }

  private def leftSubTreeHeight(node: Node[K, V]): Int = node match {
    case shadowNode: ShadowNode[K, V] =>
      val startTime = System.nanoTime()
      val restoredNode = shadowNode.restoreFullNode(avlStorage)
      restoringTime = restoringTime + (System.nanoTime() - startTime)
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
        val startTime = System.nanoTime()
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        restoringTime = restoringTime + (System.nanoTime() - startTime)
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
        var startTime = System.nanoTime()
        val prevRootWithUpdatedChildren =
          internalNode.updateChilds(newLeftChild = newLeftChildForPrevRoot)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
        val prevRoot = prevRootWithUpdatedChildren.selfInspection
        startTime = System.nanoTime()
        val newUpdatedRoot =
          newRoot.updateChilds(newRightChild = prevRoot)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
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
        val startTime = System.nanoTime()
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        restoringTime = restoringTime + (System.nanoTime() - startTime)
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
        var startTime = System.nanoTime()
        val prevRootWithUpdatedChildren =
          internalNode.updateChilds(newRightChild = newRightChildForPrevRoot)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
        val prevRoot       = prevRootWithUpdatedChildren.selfInspection
        startTime = System.nanoTime()
        val newUpdatedRoot = newRoot.updateChilds(newLeftChild = prevRoot)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
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
        val startTime = System.nanoTime()
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        restoringTime = restoringTime + (System.nanoTime() - startTime)
        rlRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val rotatedRightChild = rightRotation(internalNode.rightChild)
        var startTime = System.nanoTime()
        val updatedNode =
          internalNode.updateChilds(newRightChild = rotatedRightChild)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
        val newNodes = List(updatedNode)
        val deletedNodes = List(internalNode, internalNode.rightChild)
        //logger.info("rl rotat")
        addToStat(newNodes, deletedNodes)
        //logger.info(s"RL ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
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
        val startTime = System.nanoTime()
        val restoredNode = shadowNode.restoreFullNode(avlStorage)
        restoringTime = restoringTime + (System.nanoTime() - startTime)
        lrRotation(restoredNode)
      case leafNode: LeafNode[K, V] => leafNode
      case internalNode: InternalNode[K, V] =>
        val rotatedLeftChild = leftRotation(internalNode.leftChild)
        var startTime = System.nanoTime()
        val updatedNode =
          internalNode.updateChilds(newLeftChild = rotatedLeftChild)
        updateChilds = updateChilds + (System.nanoTime() - startTime)
        val newNodes = List(updatedNode)
        val deletedNodes = List(internalNode, internalNode.leftChild)
        //logger.info("lr rotat")
        addToStat(newNodes, deletedNodes)
        //logger.info(s"LR ROTATION: (${Algos.encode(updatedNode.hash)}) (${Algos.encode(internalNode.hash)}")
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
