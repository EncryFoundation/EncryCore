package encry.settings

import encry.storage.VersionalStorage.StorageType

case class StorageSettings(history: StorageType,
                           wallet: StorageType,
                           state: StorageType)
