package encry.storage

import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import supertagged.TaggedType

/**
  * Main trait for all versional db wrappers.
  */
trait VersionalStorage {

  def get(key: StorageKey): Option[StorageValue]

  def currentVersion: StorageVersion

  def versions: List[StorageVersion]

  def rollbackTo(to: StorageVersion)

  def insert(version: StorageVersion,
             toInsert: List[(StorageKey, StorageValue)],
             toDelete: List[StorageKey] = List.empty)

  def close(): Unit
}

object VersionalStorage {

  object StorageVersion extends TaggedType[Array[Byte]]
  object StorageKey extends TaggedType[Array[Byte]]
  object StorageValue extends TaggedType[Array[Byte]]
  object StorageType extends TaggedType[String]

  type StorageVersion = StorageVersion.Type
  type StorageKey = StorageKey.Type
  type StorageValue = StorageValue.Type
  type StorageType = StorageType.Type

  val LevelDB: StorageType = StorageType @@ "LevelDb"
  val IODB: StorageType = StorageType @@ "iodb"
}
