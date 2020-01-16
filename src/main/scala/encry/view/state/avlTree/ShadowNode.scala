package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg.NodeTypes.ShadowNodeProto
import cats.Monoid
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.StorageKey
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos

import scala.util.Try

//represent node, that stored in db, without all fields, except hash, height, balance
case class ShadowNode[K: Serializer: Hashable, V: Serializer](nodeHash: Array[Byte],
                                                              height: Int,
                                                              balance: Int)
                                                             (implicit kM: Monoid[K], vM: Monoid[V]) extends Node[K, V] with StrictLogging {

  override val key: K = kM.empty
  override val value: V = vM.empty

  override lazy val hash = nodeHash

  def restoreFullNode(storage: VersionalStorage): Node[K, V] = if (nodeHash.nonEmpty)
    NodeSerilalizer.fromBytes[K, V](
      {
        val res = storage.get(StorageKey @@ AvlTree.nodeKey(hash))
        if (res.isEmpty) logger.info(s"Empty node at key: ${Algos.encode(hash)}")
        res.get
      }
    ) else EmptyNode[K, V]()

  def tryRestore(storage: VersionalStorage): Option[Node[K, V]] =
    Try(restoreFullNode(storage)).toOption

  override def toString: String = s"ShadowNode(Hash:${Algos.encode(hash)}, height: ${height}, balance: ${balance})"

  override def selfInspection = this
}

object ShadowNode {

  def childsToShadowNode[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): Node[K, V] = node match {
    case internal: InternalNode[K, V] =>
      internal.copy(
        leftChild = ShadowNode[K, V](
            nodeHash = internal.leftChild.hash,
            height = internal.leftChild.height,
            balance = internal.leftChild.balance),
        rightChild = ShadowNode[K, V](
          nodeHash = internal.rightChild.hash,
          height = internal.rightChild.height,
          balance = internal.rightChild.balance),
      )
    case _ => node
  }

  def toProto[K: Serializer, V](node: ShadowNode[K, V]): ShadowNodeProto = ShadowNodeProto()
    .withHeight(node.height)
    .withHash(ByteString.copyFrom(node.hash))
    .withBalance(node.balance)

  def fromProto[K: Hashable : Monoid : Serializer, V: Monoid : Serializer](protoNode: ShadowNodeProto): Try[ShadowNode[K, V]] = Try {
    ShadowNode(
      nodeHash = protoNode.hash.toByteArray,
      height = protoNode.height,
      balance = protoNode.balance
    )
  }
}
