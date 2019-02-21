package encry.settings

import encry.storage.VersionalStorage.StorageType

case class StorageSettings(history: StorageType,
                           auxHistory: StorageType,
                           wallet: StorageType,
                           state: StorageType)
