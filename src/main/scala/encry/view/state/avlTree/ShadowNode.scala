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
case class ShadowNode[K: Serializer: Hashable, V: Serializer](hash: Array[Byte], height: Int, balance: Int)
                                                             (implicit kM: Monoid[K], vM: Monoid[V]) extends Node[K, V] with StrictLogging {

  override val key: K = kM.empty
  override val value: V = vM.empty

  def restoreFullNode(storage: VersionalStorage): Node[K, V] = {
    if (storage.get(StorageKey @@ hash).isEmpty) logger.info(s"Trying to get hash ${Algos.encode(hash)}, but res is null")
    NodeSerilalizer.fromBytes[K, V](
      {
        storage.get(StorageKey @@ hash).get
      }
    )
  }

  override def toString: String = s"ShadowNode(Hash:${Algos.encode(hash)}, height: ${height}, balance: ${balance})"

  override def selfInspection(prevOpsInfo: OperationInfo[K, V]): NodeWithOpInfo[K, V] = NodeWithOpInfo(this, prevOpsInfo)
}

object ShadowNode {

  def childsToShadowNode[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): Node[K, V] = node match {
    case internal: InternalNode[K, V] =>
      if (internal.leftChild.exists(leftNode => Algos.encode(leftNode.hash) == "ec905fa4cdcbca936b4e73a1265926f52d4f04d522ff7b319e19621876db7533") ||
        internal.rightChild.exists(rightCild => Algos.encode(rightCild.hash) == "ec905fa4cdcbca936b4e73a1265926f52d4f04d522ff7b319e19621876db7533")
      ) {
//        println(s"node: ${node}")
//        println("-----")
      }
      internal.copy(
        leftChild = internal.leftChild.map(node => ShadowNode[K, V](hash = node.hash, height = node.height, balance = node.balance)),
        rightChild = internal.rightChild.map(node => ShadowNode[K, V](hash = node.hash, height = node.height, balance = node.balance))
      )
    case _ => node
  }

  def toProto[K, V](node: ShadowNode[K, V]): ShadowNodeProto = ShadowNodeProto()
    .withHeight(node.height)
    .withHash(ByteString.copyFrom(node.hash))
    .withBalance(node.balance)

  def fromProto[K: Hashable : Monoid : Serializer, V: Monoid : Serializer](protoNode: ShadowNodeProto): Try[ShadowNode[K, V]] = Try {
    ShadowNode(
      hash = protoNode.hash.toByteArray,
      height = protoNode.height,
      balance = protoNode.balance
    )
  }
}
