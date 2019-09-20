package encry.view.state.avlTree

import cats.{Monoid, Order}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.state.avlTree.utils.implicits.ConvertableToStorage
import encry.view.state.avlTree.utils.implicits.Instances.Hashable

case class AvlVersionalStorage[K: Monoid, V: Monoid] (storage: VersionalStorage,
                                                      tree: AvlTree[K, V])
                                                      (implicit hash: Hashable[K],
                                                      convertK: ConvertableToStorage[K],
                                                      value: ConvertableToStorage[V]) extends VersionalStorage {

  val rootHash: Array[Byte] = hash.hash(tree.rootNode.key)

  override def get(key: StorageKey): Option[StorageValue] = storage.get(key)

  override def contains(key: StorageKey): Boolean = storage.contains(key)

  override def getAll(maxQty: Int): Iterator[(StorageKey, StorageValue)] = storage.getAll(maxQty)

  override def currentVersion: StorageVersion = storage.currentVersion

  override def versions: List[StorageVersion] = storage.versions

  override def rollbackTo(to: StorageVersion): Unit = storage.rollbackTo(to)

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey]): Unit = {

  }

  def insertWithTree(version: StorageVersion,
                     toInsert: List[(StorageKey, StorageValue)],
                     toDelete: List[StorageKey]): AvlVersionalStorage[K, V] = {
    val newTreeAfterDelitions = toDelete.foldLeft(tree){
      case (prevTree, keyToDelete) => prevTree.delete(convertK.convertToStorage(keyToDelete))
    }

    val newTreeAfterInsertions = toInsert.foldLeft(newTreeAfterDelitions){
      case (prevTree, (keyToInsert, valueToInsert)) =>
        prevTree.insert(convertK.convertToStorage(keyToInsert), value.convertToStorageValue(valueToInsert))
    }

    val newStorage = storage.insert(
      version,
      toInsert,
      toDelete
    )
    AvlVersionalStorage(storage, newTreeAfterDelitions)
  }

  override def close(): Unit = storage.close()
}

object AvlVersionalStorage {

  def apply[K: Monoid : Order, V: Monoid](storage: VersionalStorage)
                                         (implicit h: Hashable[K],
                                          c: ConvertableToStorage[K],
                                          v: ConvertableToStorage[V]): AvlVersionalStorage[K, V] = {
    AvlVersionalStorage(storage, AvlTree[K, V]())
  }
}
