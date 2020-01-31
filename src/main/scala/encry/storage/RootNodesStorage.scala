package encry.storage

import cats.kernel.{Monoid, Order}
import com.google.common.primitives.Ints
import encry.settings.Settings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.state.avlTree.{AvlTree, EmptyNode, Node, NodeSerilalizer}
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.encryfoundation.common.utils.constants.Constants
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.utils.Random

import scala.util.Try

trait RootNodesStorage[K, V] {

  def safePointHeight: Height

  def insert(version: StorageVersion, rootNode: Node[K, V], height: Height): RootNodesStorage[K, V]

  def rollbackToSafePoint(insertionInfo: List[(Height, (List[(K, V)], List[K]))]): (RootNodesStorage[K, V], Node[K, V])
}

object RootNodesStorage {

  def apply[K: Serializer : Monoid : Hashable : Order,
    V: Serializer : Monoid : Hashable](storage: DB, roolbackDepth: Int): RootNodesStorage[K, V] = new RootNodesStorage[K, V] {

    private def atHeightKey(height: Height): Array[Byte] = Ints.toByteArray(height)

    private val safePointKey = Algos.hash("SafePoint")

    def safePointHeight: Height = Height @@ Try(Ints.fromByteArray(storage.get(safePointKey))).getOrElse(0)

    override def insert(version: StorageVersion,
                        rootNode: Node[K, V],
                        height: Height): RootNodesStorage[K, V] =
      if (height % roolbackDepth != 0) this
      else {
        val batch = storage.createWriteBatch()
        val readOptions = new ReadOptions()
        readOptions.snapshot(storage.getSnapshot)
        try {
          val newSafePointHeight = Math.max(0, height - roolbackDepth)
          val newSafePointSerialized = Ints.toByteArray(newSafePointHeight)
          batch.put(safePointKey, newSafePointSerialized)
          batch.put(Ints.toByteArray(height), NodeSerilalizer.toBytes(rootNode))
          batch.delete(Ints.toByteArray(newSafePointHeight - roolbackDepth))
          storage.write(batch)
          batch.close()
        }
        this
      }

    override def rollbackToSafePoint(insertionInfo: List[(Height, (List[(K, V)], List[K]))]): (RootNodesStorage[K, V], Node[K, V]) = Try {
      val batch = storage.createWriteBatch()
      val readOptions = new ReadOptions()
      readOptions.snapshot(storage.getSnapshot)
      val restoredRootNode: Node[K, V] = NodeSerilalizer.fromBytes(storage.get(Ints.toByteArray(safePointHeight)))
      val avlTree = new AvlTree[K, V](restoredRootNode, EmptyVersionalStorage(), emptyRootStorage)
      val newRootNode = insertionInfo.foldLeft(avlTree) {
        case (tree, (height, (toInsert, toDelete))) =>
          val newTree = tree.insertAndDeleteMany(
            StorageVersion @@ Random.randomBytes(),
            toInsert,
            toDelete
          )
          if (height == safePointHeight + roolbackDepth) {
            batch.put(Ints.toByteArray(safePointHeight + roolbackDepth), NodeSerilalizer.toBytes(newTree.rootNode))
          }
          newTree
      }.rootNode
      this -> newRootNode
    }.get
  }

  def emptyRootStorage[K: Serializer: Monoid, V: Serializer : Monoid]: RootNodesStorage[K, V] = new RootNodesStorage[K, V] {
    override def safePointHeight: Height = Height @@ 0

    override def insert(version: StorageVersion, rootNode: Node[K, V], height: Height): RootNodesStorage[K, V] = this

    override def rollbackToSafePoint(insertionInfo: List[(Height, (List[(K, V)], List[K]))]): (RootNodesStorage[K, V], Node[K, V]) =
      this -> EmptyNode[K, V]()
  }

  def blocks2InsInfo[K, V](blocks: List[Block])(implicit kSer: Serializer[K], vSer: Serializer[V]): List[(Height, (List[(K, V)], List[K]))] =
    blocks.foldLeft(List.empty[(Height, (List[(K, V)], List[K]))]) { case (info, block) =>
      info :+ (Height @@ block.header.height,
        (block.payload.txs.flatMap(_.newBoxes.map(bx => kSer.fromBytes(bx.id) -> vSer.fromBytes(bx.bytes))).toList,
        block.payload.txs.flatMap(_.inputs.map(input => kSer.fromBytes(input.boxId))).toList)
      )
    }
}
