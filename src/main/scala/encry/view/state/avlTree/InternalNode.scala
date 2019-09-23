package encry.view.state.avlTree

import Node.NodeProtoMsg.NodeTypes.{InternalNodeProto, ShadowNodeProto}
import cats.kernel.Monoid
import com.google.protobuf.ByteString
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import org.encryfoundation.common.utils.Algos

import scala.util.Try

final case class InternalNode[K: Hashable, V](key: K,
                                              value: V,
                                              height: Int,
                                              balance: Int,
                                              leftChild: Option[Node[K, V]],
                                              rightChild: Option[Node[K, V]],
                                              hash: Array[Byte]) extends Node[K, V] {

  override def selfInspection: Node[K, V] = if (leftChild.isEmpty & rightChild.isEmpty) LeafNode(key, value)
                                            else this

  def updateChilds(newLeftChild: Option[Node[K, V]] = leftChild,
                   newRightChild: Option[Node[K, V]] = rightChild): InternalNode[K, V] = {
    val newLeftChildAfterInspect = newLeftChild.map(_.selfInspection)
    val newRightChildAfterInspect = newRightChild.map(_.selfInspection)
    this.copy(
      leftChild = newLeftChildAfterInspect,
      rightChild = newRightChildAfterInspect,
      balance = newLeftChildAfterInspect.map(_.height).getOrElse(-1) - newRightChildAfterInspect.map(_.height).getOrElse(-1),
      height = Math.max(newLeftChildAfterInspect.map(_.height).getOrElse(-1), newRightChildAfterInspect.map(_.height).getOrElse(0)) + 1,
      hash = Algos.hash(hash ++
        newLeftChildAfterInspect.map(_.hash).getOrElse(Array.emptyByteArray) ++
        newRightChildAfterInspect.map(_.hash).getOrElse(Array.emptyByteArray))
    )
  }

  override def toString: String = s"[($key," +
    s" $value," +
    s" height: $height," +
    s" balance $balance, " +
    s" hash: ${Algos.encode(hash)}) \n-> LeftChildOf($key):${leftChild.map(_.toString)}, \n-> RightChildOf($key): ${rightChild.map(_.toString)}]"
}

object InternalNode {

  def toProto[K, V](node: InternalNode[K, V])(implicit kSer: Serializer[K], vSer: Serializer[V]): InternalNodeProto = {
    val msg = InternalNodeProto()
      .withBalance(node.balance)
      .withHash(ByteString.copyFrom(node.hash))
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
      withLeftChild.withLeftChild(shadowNode)
    }.getOrElse(withLeftChild)
  }

  def fromProto[K: Monoid : Hashable, V: Monoid](protoInternal: InternalNodeProto)(implicit kSer: Serializer[K],
                                                                vSer: Serializer[V]): Try[InternalNode[K, V]] = Try {
    InternalNode[K, V](
      key = kSer.fromBytes(protoInternal.key.toByteArray),
      value = vSer.fromBytes(protoInternal.value.toByteArray),
      height = protoInternal.height,
      balance = protoInternal.balance,
      hash = protoInternal.hash.toByteArray,
      leftChild = protoInternal.leftChild.map(elem => ShadowNode.fromProto(elem).get),
      rightChild = protoInternal.rightChild.map(elem => ShadowNode.fromProto(elem).get)
    )
  }

  def apply[K, V](key: K,
                  value: V,
                  height: Int,
                  balance: Int)(implicit hash: Hashable[K]): InternalNode[K, V] =
    new InternalNode(key, value, height, balance, None, None, hash = hash.hash(key))
}
