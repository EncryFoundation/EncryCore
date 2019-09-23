package encry.view.state.avlTree

import Node.NodeProtoMsg.NodeTypes.LeafNodeProto
import com.google.protobuf.ByteString
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import org.encryfoundation.common.utils.Algos

import scala.util.Try

final case class LeafNode[K, V](key: K,
                                value: V)(implicit hashK: Hashable[K]) extends Node[K, V] {

  override val balance: Int = 0

  override val height: Int = 0

  override def selfInspection: Node[K, V] = this

  override val hash: Array[Byte] = hashK.hash(key)

  override def toString: String = s"($key, $value, height: 0, balance: 0, hash: ${Algos.encode(hash)})"

}

object LeafNode {
  def toProto[K, V](leaf: LeafNode[K, V])(implicit kSer: Serializer[K],
                                          vSer: Serializer[V]): LeafNodeProto = LeafNodeProto()
    .withKey(ByteString.copyFrom(kSer.toBytes(leaf.key)))
    .withValue(ByteString.copyFrom(vSer.toBytes(leaf.value)))

  def fromProto[K: Hashable, V](leafProto: LeafNodeProto)(implicit kSer: Serializer[K],
                                                          vSer: Serializer[V]): Try[LeafNode[K, V]] = Try {
    LeafNode(
      kSer.fromBytes(leafProto.key.toByteArray),
      vSer.fromBytes(leafProto.value.toByteArray)
    )
  }
}
