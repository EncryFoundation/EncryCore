package encry.storage

import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import org.rocksdb.{ReadOptions, RocksDB, WriteBatch, WriteOptions}

case class LevelDBStorage(db: RocksDB) extends VersionalStorage {

  override def get(key: StorageKey): Option[StorageValue] = {
    val possibleRes = db.get(key)
    if (possibleRes == null) None else Some(StorageValue @@ possibleRes)
  }

  override def contains(key: StorageKey): Boolean = {
    val possibleRes = db.get(key)
    if (possibleRes == null) false else true
  }

  override def getAll(maxQty: Int): Iterator[(StorageKey, StorageValue)] = Iterator.empty

  override def getAllKeys(maxQty: Int): Iterator[StorageKey] = Iterator.empty

  override def currentVersion: StorageVersion = StorageVersion @@ Array.emptyByteArray

  override def versions: List[StorageVersion] = List.empty

  override def rollbackTo(to: StorageVersion): Unit = ()

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey]): Unit = {
    val readOptions = new ReadOptions()
    val batch = new WriteBatch()
    val writeOptions = new WriteOptions()
    readOptions.setSnapshot(db.getSnapshot)
    toInsert.foreach { case (key, value) =>
      batch.put(key, value)
    }
    toDelete foreach batch.delete
    db.write(writeOptions, batch)
    batch.close()
  }

  override def close(): Unit = db.close()
}
