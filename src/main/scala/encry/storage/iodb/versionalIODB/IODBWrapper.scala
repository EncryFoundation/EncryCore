package encry.storage.iodb.versionalIODB

import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import io.iohk.iodb.Store.{K, V}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.utils.Algos

import scala.collection.mutable

/**
  * Wrapper, which extends VersionalStorage trait
  * @param store
  */
case class IODBWrapper(store: Store) extends VersionalStorage with StrictLogging with AutoCloseable {

  override def get(key: StorageKey): Option[StorageValue] =
    store.get(ByteArrayWrapper(key)).map(StorageValue @@ _.data)

  override def contains(key: StorageKey): Boolean = get(key).isDefined

  override def currentVersion: StorageVersion =
    store.lastVersionID.map(StorageVersion @@ _.data).getOrElse(IODBWrapper.initVer)

  override def versions: List[StorageVersion] =
    store.rollbackVersions().map(StorageVersion @@ _.data).toList

  override def rollbackTo(to: StorageVersion): Unit =
    store.rollback(ByteArrayWrapper(to))

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey] = List.empty): Unit = {
    logger.info(s"Update to version: ${Algos.encode(version)}")
    store.update(
      ByteArrayWrapper(version),
      toDelete.map(ByteArrayWrapper.apply),
      toInsert.map{case (keyToAdd, valToAdd) => ByteArrayWrapper(keyToAdd) -> ByteArrayWrapper(valToAdd)}
    )
  }

  //always return all elements
  override def getAll(maxQty: Int = -1): Iterator[(StorageKey, StorageValue)] =
    store.getAll().map{case (key, value) => StorageKey @@ key.data -> StorageValue @@ value.data}

  override def getAllKeys(maxQty: Int = -1): Iterator[StorageKey] =
    store.getAll().map{case (key, _) => StorageKey @@ key.data}

  override def close(): Unit = store.close()
}

object IODBWrapper {

  val initVer: StorageVersion = StorageVersion @@ Array.fill(33)(0: Byte)
}
