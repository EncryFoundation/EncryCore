package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg.NodeTypes.{InternalNodeProto, ShadowNodeProto}
import cats.Monoid
import com.google.protobuf.ByteString
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos

import scala.util.Try

final case class InternalNode[K: Hashable, V](key: K,
                                              value: V,
                                              height: Int,
                                              balance: Int,
                                              leftChild: Option[Node[K, V]],
                                              rightChild: Option[Node[K, V]],
                                              hash: Array[Byte]) extends Node[K, V] {

  override def selfInspection(prevOpsInfo: OperationInfo[K, V]): NodeWithOpInfo[K, V] =
    if (leftChild.isEmpty & rightChild.isEmpty) {
      val leaf = LeafNode(key, value)
      NodeWithOpInfo(leaf, prevOpsInfo.update(ByteArrayWrapper(leaf.hash) -> leaf, ByteArrayWrapper(hash)))
    }
    else NodeWithOpInfo(this, prevOpsInfo)

  def updateChilds(newLeftChild: Option[Node[K, V]] = leftChild,
                   newRightChild: Option[Node[K, V]] = rightChild,
                   prevOpsInfo: OperationInfo[K, V]): NodeWithOpInfo[K, V] = {
    val (newLeftChildAfterInspect, leftInfo) =
      newLeftChild.map{node =>
        val resOp = node.selfInspection(prevOpsInfo)
        (Some(resOp.node), resOp.opInfo)
      }.getOrElse(Option.empty[Node[K, V]], prevOpsInfo)
    val (newRightChildAfterInspect, rightInfo) =
      newRightChild.map{node =>
        val resOp = node.selfInspection(leftInfo)
        (Some(resOp.node), resOp.opInfo)
      }.getOrElse(Option.empty[Node[K, V]], leftInfo)
    val newNode = this.copy(
      leftChild = newLeftChildAfterInspect,
      rightChild = newRightChildAfterInspect,
      balance = newLeftChildAfterInspect.map(_.height).getOrElse(-1) - newRightChildAfterInspect.map(_.height).getOrElse(-1),
      height = Math.max(newLeftChildAfterInspect.map(_.height).getOrElse(-1), newRightChildAfterInspect.map(_.height).getOrElse(0)) + 1,
      hash = Algos.hash(hash ++
        newLeftChildAfterInspect.map(_.hash).getOrElse(Array.emptyByteArray) ++
        newRightChildAfterInspect.map(_.hash).getOrElse(Array.emptyByteArray))
    )
    val newUpdated =
      if (newNode.hash sameElements this.hash) rightInfo else rightInfo.update(ByteArrayWrapper(newNode.hash) -> newNode, ByteArrayWrapper(this.hash))
    NodeWithOpInfo(newNode, newUpdated)
  }

  override def toString: String = {
//    implicit val kTOStr: Hashable[K] = new Hashable[K] {
//      override def hash(value: K): Array[Byte] = value.asInstanceOf[StorageKey]
//    }
//    implicit val vTOStr: Hashable[V] = new Hashable[V] {
//      override def hash(value: V): Array[Byte] = value.asInstanceOf[StorageValue]
//    }
    s"[(${Algos.encode(key.asInstanceOf[StorageKey])}," +
      s" ${Algos.encode(value.asInstanceOf[StorageValue])}," +
      s" height: $height," +
      s" balance $balance, " +
      s" hash: ${Algos.encode(hash)}) \n-> LeftChildOf(${Algos.encode(key.asInstanceOf[StorageKey])}):${leftChild.map(_.toString)}, " +
      s"\n-> RightChildOf(${Algos.encode(key.asInstanceOf[StorageKey])}): ${rightChild.map(_.toString)}]"
  }
}

object InternalNode {

  def toProto[K, V](node: InternalNode[K, V])(implicit kSer: Serializer[K], vSer: Serializer[V]): InternalNodeProto = {
    val msg = InternalNodeProto()
      .withBalance(node.balance)
      .withHash(ByteString.copyFrom(node.hash))
      .withHeight(node.height)
      .withKey(ByteString.copyFrom(kSer.toBytes(node.key)))
      .withValue(ByteString.copyFrom(vSer.toBytes(node.value)))
    val withLeftChild = node.leftChild.map { leftChild =>
      val shadowNode = ShadowNodeProto()
          .withBalance(leftChild.balance)
          .withHash(ByteString.copyFrom(leftChild.hash))
          .withHeight(leftChild.height)
      msg.withLeftChild(shadowNode)
    }.getOrElse(msg)
    node.rightChild.map { rightChild =>
      val shadowNode = ShadowNodeProto()
        .withBalance(rightChild.balance)
        .withHash(ByteString.copyFrom(rightChild.hash))
        .withHeight(rightChild.height)
      withLeftChild.withRightChild(shadowNode)
    }.getOrElse(withLeftChild)
  }

  def fromProto[K: Monoid : Hashable, V: Monoid](protoInternal: InternalNodeProto)
                                                (implicit kSer: Serializer[K],
                                                vSer: Serializer[V]): Try[InternalNode[K, V]] = Try {
    new InternalNode(
      key = kSer.fromBytes(protoInternal.key.toByteArray),
      value = vSer.fromBytes(protoInternal.value.toByteArray),
      height = protoInternal.height,
      balance = protoInternal.balance,
      hash = protoInternal.hash.toByteArray,
      leftChild = protoInternal.leftChild.map(elem => ShadowNode.fromProto[K, V](elem).get),
      rightChild = protoInternal.rightChild.map(elem => ShadowNode.fromProto[K, V](elem).get)
    )
  }

  def apply[K, V](key: K,
                  value: V,
                  height: Int,
                  balance: Int)(implicit hash: Hashable[K]): InternalNode[K, V] =
    new InternalNode(key, value, height, balance, None, None, hash = hash.hash(key))
}
