package encry.storage.iodb.versionalIODB

import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import io.iohk.iodb.Store
import org.encryfoundation.common.Algos

/**
  * Iodb db wrapper with object store, only for history
  * @param store
  * @param objectStore
  */
case class IODBWrapperForHistory(store: Store, objectStore: Store) extends VersionalStorage {

  private val iodbWrapper = IODBWrapper(store)

  override def get(key: StorageKey): Option[StorageValue] = iodbWrapper.get(key)

  override def getAll(): Iterator[(StorageKey, StorageValue)] = iodbWrapper.getAll()

  override def currentVersion: StorageVersion = iodbWrapper.currentVersion

  override def versions: List[StorageVersion] = iodbWrapper.versions

  override def rollbackTo(to: StorageVersion): Unit = iodbWrapper.rollbackTo(to)

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey]): Unit = {
    if (toDelete.nonEmpty) iodbWrapper.insert(version, List.empty, toDelete)
    iodbWrapper.insert(StorageVersion !@@ Algos.hash(version), toInsert)
  }

  override def close(): Unit = {
    iodbWrapper.close()
    objectStore.close()
  }
}