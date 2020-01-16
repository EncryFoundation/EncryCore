package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg.NodeTypes.LeafNodeProto
import cats.Monoid
import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import encry.view.state.avlTree.utils.implicits.{ Hashable, Serializer }
import org.encryfoundation.common.utils.Algos
import scala.util.Try

final case class LeafNode[K: Serializer: Monoid, V: Serializer: Monoid](key: K, value: V)(implicit hashK: Hashable[K])
    extends Node[K, V] {

  override lazy val hash: Array[Byte] = Algos.hash(
    Bytes.concat(
      implicitly[Serializer[K]].toBytes(key),
      implicitly[Serializer[V]].toBytes(value)
    )
  )

  override val balance: Int = 0

  override val height: Int = 0

  override def toString: String = s"($key, $value, height: 0, balance: 0, hash: ${Algos.encode(hash)})"

  override def selfInspection = this
}

object LeafNode {
  def toProto[K, V](leaf: LeafNode[K, V])(implicit kSer: Serializer[K], vSer: Serializer[V]): LeafNodeProto =
    LeafNodeProto().withKey(ByteString.copyFrom(kSer.toBytes(leaf.key)))

  def fromProto[K: Hashable: Monoid, V: Monoid](
    leafProto: LeafNodeProto
  )(implicit kSer: Serializer[K], vSer: Serializer[V]): Try[LeafNode[K, V]] = Try {
    LeafNode(
      kSer.fromBytes(leafProto.key.toByteArray),
      vSer.fromBytes(leafProto.value.toByteArray),
    )
  }
}
