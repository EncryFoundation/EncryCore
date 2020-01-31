package encry.storage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}

case class EmptyVersionalStorage() extends VersionalStorage {

  override def get(key: StorageKey): Option[StorageValue] = None

  override def contains(key: StorageKey): Boolean = false

  override def getAll(maxQty: Int): Iterator[(StorageKey, StorageValue)] = Iterator.empty

  override def getAllKeys(maxQty: Int): Iterator[StorageKey] = Iterator.empty

  override def currentVersion: StorageVersion = StorageVersion @@ Array.emptyByteArray

  override def versions: List[StorageVersion] = List.empty

  override def rollbackTo(to: StorageVersion): Unit = ()

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey]): Unit = ()

  override def close(): Unit = ()
}
