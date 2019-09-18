package encry.view.state.avlTree

import encry.storage.VersionalStorage

trait AvlVersionalStorage extends VersionalStorage {

  def storage: AvlVersionalStorage

  def tree: AvlTree

  def root: Array[Byte]
}
