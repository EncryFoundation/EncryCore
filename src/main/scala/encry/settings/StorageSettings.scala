package encry.settings

import encry.storage.VersionalStorage.StorageType

case class StorageSettings(history: StorageType,
                           auxHistory: StorageType,
                           state: StorageType)

object StorageSettings {

  def apply(history: StorageType,
            auxHistory: StorageType,
            state: StorageType): StorageSettings =
    new StorageSettings(history, auxHistory, state)
}