package encry.storage

import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import supertagged.TaggedType

/**
  * Main trait for all versional db wrappers.
  */
trait VersionalStorage {

  def get(key: StorageKey): Option[StorageValue]

  def contains(key: StorageKey): Boolean

  def getAll(maxQty: Int): Iterator[(StorageKey, StorageValue)]

  def currentVersion: StorageVersion

  def versions: List[StorageVersion]

  def contains(key: StorageKey): Boolean

  def rollbackTo(to: StorageVersion)

  def insert(version: StorageVersion,
             toInsert: List[(StorageKey, StorageValue)],
             toDelete: List[StorageKey] = List.empty)

  def close(): Unit
}

object VersionalStorage {

  sealed trait StorageType
  case object IODB extends StorageType
  case object LevelDB extends StorageType

  object StorageVersion extends TaggedType[Array[Byte]]
  object StorageKey extends TaggedType[Array[Byte]]
  object StorageValue extends TaggedType[Array[Byte]]

  type StorageVersion = StorageVersion.Type
  type StorageKey = StorageKey.Type
  type StorageValue = StorageValue.Type

}
