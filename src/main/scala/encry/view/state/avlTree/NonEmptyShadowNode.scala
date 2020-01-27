package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg.NodeTypes.{NonEmptyShadowNodeProto, ShadowNodeProto}
import cats.Monoid
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.StorageKey
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import org.encryfoundation.common.utils.Algos

import scala.util.Try

case class NonEmptyShadowNode[K: Serializer: Hashable, V: Serializer](nodeHash: Array[Byte],
                                                                      height: Int,
                                                                      balance: Int,
                                                                      key: K)
                                                                     (implicit kM: Monoid[K], vM: Monoid[V]) extends ShadowNode[K, V] with StrictLogging {

  override val value: V = vM.empty

  override val hash = nodeHash

  def restoreFullNode(storage: VersionalStorage): Node[K, V] = if (nodeHash.nonEmpty) {
    NodeSerilalizer.fromBytes[K, V](
      {
        val res = storage.get(StorageKey @@ AvlTree.nodeKey(hash))
        if (res.isEmpty) logger.info(s"Empty node at key: ${Algos.encode(hash)}")
        res.get
      }
    )
  } else EmptyNode[K, V]()

  def tryRestore(storage: VersionalStorage): Option[Node[K, V]] =
    Try(restoreFullNode(storage)).toOption

  override def toString: String = s"ShadowNode(Hash:${Algos.encode(hash)}, height: ${height}, balance: ${balance})"
}

object NonEmptyShadowNode {

  def toProto[K: Serializer, V](node: NonEmptyShadowNode[K, V]): NonEmptyShadowNodeProto = NonEmptyShadowNodeProto()
    .withHeight(node.height)
    .withHash(ByteString.copyFrom(node.hash))
    .withBalance(node.balance)
    .withKey(ByteString.copyFrom(implicitly[Serializer[K]].toBytes(node.key)))

  def fromProto[K: Hashable : Monoid : Serializer, V: Monoid : Serializer](protoNode: NonEmptyShadowNodeProto): Try[NonEmptyShadowNode[K, V]] = Try {
    NonEmptyShadowNode(
      nodeHash = protoNode.hash.toByteArray,
      height = protoNode.height,
      balance = protoNode.balance,
      key = implicitly[Serializer[K]].fromBytes(protoNode.key.toByteArray)
    )
  }
}


