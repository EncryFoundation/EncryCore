package encry.view.state.avlTree

import encry.storage.VersionalStorage

trait AvlVersionalStorage[K, V] extends VersionalStorage {

  def storage: VersionalStorage

  def tree[K, V]: AvlTree[K, V]

  def root: K
}
