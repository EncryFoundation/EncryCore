package encry.storage.levelDb.versionalLevelDB

import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}

case class VLDBWrapper(vldb: VersionalLevelDB) extends VersionalStorage with AutoCloseable {

  override def get(key: StorageKey): Option[StorageValue] =
    vldb.get(VersionalLevelDbKey @@ key.untag(StorageKey)).map(StorageValue @@ _.untag(VersionalLevelDbValue))

  override def currentVersion: StorageVersion =
    StorageVersion @@ vldb.currentVersion.untag(LevelDBVersion)

  override def versions: List[StorageVersion] =
    vldb.versionsList.map(StorageVersion @@ _.untag(LevelDBVersion))

  override def contains(key: StorageKey): Boolean = vldb.contains(VersionalLevelDbKey @@ key.untag(StorageKey))

  override def rollbackTo(to: StorageVersion): Unit =
    vldb.rollbackTo(LevelDBVersion @@ to.untag(StorageVersion))

  override def insert(version: StorageVersion,
                      toInsert: List[(StorageKey, StorageValue)],
                      toDelete: List[StorageKey] = List.empty): Unit = {
    vldb.insert(
      LevelDbDiff(
        LevelDBVersion @@ version.untag(StorageVersion),
        toInsert.map{case (key, value) =>
          VersionalLevelDbKey @@ key.untag(StorageKey) -> VersionalLevelDbValue @@ value.untag(StorageValue)
        },
        toDelete.map(VersionalLevelDbKey @@ _.untag(StorageKey))
      )
    )
  }

  override def getAllKeys(maxQty: Int = -1): Iterator[StorageKey] = {
    vldb.getCurrentElementsKeys(maxQty).map { key => StorageKey @@ key.untag(VersionalLevelDbKey) }
  }.toIterator

  //by default return all elems
  override def getAll(maxQty: Int = -1): Iterator[(StorageKey, StorageValue)] =
    vldb.getAll(maxQty).map{ case (key, value) =>
      StorageKey @@ key.untag(VersionalLevelDbKey) -> StorageValue @@ value.untag(VersionalLevelDbValue)
    }.toIterator

  override def close(): Unit = vldb.close()
}