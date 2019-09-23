package encry.view.state.avlTree

import Node.NodeProtoMsg.NodeTypes.ShadowNodeProto
import cats.Monoid
import com.google.protobuf.ByteString
import encry.storage.VersionalStorage

import scala.util.Try

//represent node, that stored in db, without all fields, except hash, height, balance
case class ShadowNode[K, V](hash: Array[Byte], height: Int, balance: Int)
                           (implicit kM: Monoid[K], vM: Monoid[V]) extends Node[K, V]{

  override val key: K = kM.empty
  override val value: V = vM.empty

  def restoreFullNode(storage: VersionalStorage): Node[K, V] = this

  override def selfInspection: Node[K, V] = this
}

object ShadowNode {
  def toProto[K, V](node: ShadowNode[K, V]): ShadowNodeProto = ShadowNodeProto()
    .withHeight(node.height)
    .withHash(ByteString.copyFrom(node.hash))
    .withBalance(node.balance)
  def fromProto[K: Monoid, V: Monoid](protoNode: ShadowNodeProto): Try[ShadowNode[K, V]] = Try {
    ShadowNode(
      hash = protoNode.hash.toByteArray,
      height = protoNode.height,
      balance = protoNode.balance
    )
  }
}
