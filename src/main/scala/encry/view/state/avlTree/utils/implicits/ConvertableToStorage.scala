package encry.view.state.avlTree.utils.implicits

import encry.storage.VersionalStorage.{StorageKey, StorageValue}

trait ConvertableToStorage[K] {

  def convertToStorage(firstValue: StorageKey): K
  def convertToStorageValue(firstValue: StorageValue): K = convertToStorage(StorageKey !@@ firstValue)
}
