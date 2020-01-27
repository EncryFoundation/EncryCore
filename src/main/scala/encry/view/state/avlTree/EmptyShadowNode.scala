package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg.NodeTypes.EmptyShadowNodeProto
import cats.kernel.Monoid
import encry.storage.VersionalStorage
import encry.view.state.avlTree.utils.implicits.Serializer

class EmptyShadowNode[K: Serializer: Monoid, V: Serializer: Monoid] extends ShadowNode[K, V] {
  override val key: K = implicitly[Monoid[K]].empty
  override val value: V = implicitly[Monoid[V]].empty
  override val height: Int = -1
  override val balance: Int = 0
  override def hash: Array[Byte] = Array.emptyByteArray
  override def restoreFullNode(storage: VersionalStorage): Node[K, V] = EmptyNode[K, V]
}

object EmptyShadowNode {
  def toProto: EmptyShadowNodeProto = EmptyShadowNodeProto()
  def fromProto[K: Serializer: Monoid, V: Serializer: Monoid](proto: EmptyShadowNodeProto): EmptyShadowNode[K, V] =
    new EmptyShadowNode[K, V]
}
