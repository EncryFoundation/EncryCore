package encry.storage.iodb.versionalIODB

import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import io.iohk.iodb.{ByteArrayWrapper, Store}

/**
  * Wrapper, which extends VersionalStorage trait
  * @param store
  */
case class IODBWrapper(store: Store) extends VersionalStorage {

  override def get(key: StorageKey): Option[StorageValue] =
    store.get(ByteArrayWrapper(key)).map(StorageValue @@ _.data)

  override def currentVersion: StorageVersion =
    store.lastVersionID.map(StorageVersion @@ _.data).getOrElse(IODBWrapper.initVer)

  override def versions: List[StorageVersion] =
    store.rollbackVersions().map(StorageVersion @@ _.data).toList

  override def rollbackTo(to: StorageVersion): Unit =
    store.rollback(ByteArrayWrapper(to))

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey] = List.empty): Unit = {
    store.update(
      ByteArrayWrapper(version),
      toDelete.map(ByteArrayWrapper.apply),
      toInsert.map{case (keyToAdd, valToAdd) => ByteArrayWrapper(keyToAdd) -> ByteArrayWrapper(valToAdd)}
    )
  }

  override def close(): Unit = store.close()
}

object IODBWrapper {

  val initVer: StorageVersion = StorageVersion @@ Array.fill(33)(0: Byte)
}
