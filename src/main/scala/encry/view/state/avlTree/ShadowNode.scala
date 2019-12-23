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
case class ShadowNode[K: Serializer: Hashable, V: Serializer](val nodeHash: Array[Byte],
                                                              height: Int,
                                                              balance: Int)
                                                             (implicit kM: Monoid[K], vM: Monoid[V]) extends Node[K, V] with StrictLogging {

  override val key: K = kM.empty
  override val value: V = vM.empty

  override lazy val hash = nodeHash

  def restoreFullNode(storage: VersionalStorage): Node[K, V] = {
    NodeSerilalizer.fromBytes[K, V](
      {
        val res = storage.get(StorageKey @@ hash)
        if (res.isEmpty) logger.info(s"Empty node at key: ${Algos.encode(hash)}")
        res.get
      }
    )
  }

  def tryRestore(storage: VersionalStorage): Option[Node[K, V]] =
    Try(restoreFullNode(storage)).toOption

  override def toString: String = s"ShadowNode(Hash:${Algos.encode(hash)}, height: ${height}, balance: ${balance})"

  override def selfInspection = this
}

object ShadowNode {

  def childsToShadowNode[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): Node[K, V] = node match {
    case internal: InternalNode[K, V] =>
      internal.copy(
        leftChild = internal.leftChild.map(node =>
          ShadowNode[K, V](
            nodeHash = node.hash,
            height = node.height,
            balance = node.balance,
          )),
        rightChild = internal.rightChild.map(node =>
          ShadowNode[K, V](
            nodeHash = node.hash,
            height = node.height,
            balance = node.balance,
          )
        )
      )
    case _ => node
  }

  def getLeftChildHash[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): Option[Array[Byte]] =
    node match {
      case internalNode: InternalNode[K, V] =>internalNode.leftChild.map(_.hash)
      case _ => None
    }

  def getRightChildHash[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): Option[Array[Byte]] =
    node match {
      case internalNode: InternalNode[K, V] => internalNode.rightChild.map(_.hash)
      case _ => None
    }

  def toProto[K, V](node: ShadowNode[K, V]): ShadowNodeProto = ShadowNodeProto()
    .withHeight(node.height)
    .withHash(ByteString.copyFrom(node.hash))
    .withBalance(node.balance)

  def fromProto[K: Hashable : Monoid : Serializer, V: Monoid : Serializer](protoNode: ShadowNodeProto): Try[ShadowNode[K, V]] = Try {
    ShadowNode(
      nodeHash = protoNode.hash.toByteArray,
      height = protoNode.height,
      balance = protoNode.balance,
    )
  }
}
