package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg
import NodeMsg.NodeProtoMsg.NodeTypes.{InternalNodeProto, ShadowNodeProto}
import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import com.google.protobuf.ByteString
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos

import scala.util.Try

final case class InternalNode[K: Serializer: Monoid: Hashable, V: Serializer: Monoid](key: K,
                                                                                      value: V,
                                                                                      height: Int,
                                                                                      balance: Int,
                                                                                      leftChild: Option[Node[K, V]],
                                                                                      rightChild: Option[Node[K, V]])
    extends Node[K, V] {

  override lazy val hash: Array[Byte] = {
    val serK = implicitly[Serializer[K]]
    val serV = implicitly[Serializer[V]]
    Algos.hash(
      Bytes.concat(serK.toBytes(key),
        serV.toBytes(value),
        Ints.toByteArray(height),
        Ints.toByteArray(balance),
        leftChild.map(_.hash).getOrElse(Array.emptyByteArray),
        rightChild.map(_.hash).getOrElse(Array.emptyByteArray)
      )
    )
  }

  override def selfInspection: Node[K, V] =
    if (leftChild.isEmpty & rightChild.isEmpty) LeafNode(key, value)
    else this

  def updateChilds(newLeftChild: Option[Node[K, V]] = leftChild,
                   newRightChild: Option[Node[K, V]] = rightChild): Node[K, V] = {
    val hashK = implicitly[Hashable[K]]
    val newLeftChildAfterInspect =
      newLeftChild.map { node =>
        val resOp = node.selfInspection
        Some(resOp)
      }.getOrElse(Option.empty[Node[K, V]])
    val newRightChildAfterInspect =
      newRightChild.map { node =>
        val resOp = node.selfInspection
        Some(resOp)
      }.getOrElse(Option.empty[Node[K, V]])
    this.copy(
      leftChild = newLeftChildAfterInspect,
      rightChild = newRightChildAfterInspect,
      balance = newLeftChildAfterInspect
        .map(_.height)
        .getOrElse(-1) - newRightChildAfterInspect.map(_.height).getOrElse(-1),
      height = Math.max(newLeftChildAfterInspect.map(_.height).getOrElse(-1),
                        newRightChildAfterInspect.map(_.height).getOrElse(0)) + 1
    )
  }

  override def toString: String =
    s"[($key," +
      s" $value," +
      s" height: $height," +
      s" balance $balance, " +
      s" hash: ${Algos.encode(hash)}) \n-> LeftChildOf(${Algos.encode(implicitly[Serializer[K]].toBytes(key))}):" +
      s"${leftChild.map(_.toString)}, \n-> RightChildOf(${Algos.encode(implicitly[Serializer[K]].toBytes(key))}): ${rightChild
        .map(_.toString)}]"
}

object InternalNode {

  def toProto[K, V](node: InternalNode[K, V])(implicit kSer: Serializer[K], vSer: Serializer[V]): InternalNodeProto = {
    val msg = InternalNodeProto()
      .withBalance(node.balance)
      .withHeight(node.height)
      .withKey(ByteString.copyFrom(kSer.toBytes(node.key)))
      .withValue(ByteString.copyFrom(vSer.toBytes(node.value)))
    val withLeftChild = node.leftChild.map { leftChild =>
      val child: NodeProtoMsg = NodeSerilalizer.toProto(leftChild)
      msg.withLeftChild(child)
    }.getOrElse(msg)
    node.rightChild.map { rightChild =>
      val child: NodeProtoMsg = NodeSerilalizer.toProto(rightChild)
      withLeftChild.withRightChild(child)
    }.getOrElse(withLeftChild)
  }

  def fromProto[K: Monoid: Hashable, V: Monoid](
    protoInternal: InternalNodeProto
  )(implicit kSer: Serializer[K], vSer: Serializer[V]): Try[InternalNode[K, V]] = Try {
    new InternalNode(
      key = kSer.fromBytes(protoInternal.key.toByteArray),
      value = vSer.fromBytes(protoInternal.value.toByteArray),
      height = protoInternal.height,
      balance = protoInternal.balance,
      leftChild = protoInternal.leftChild.map(elem => NodeSerilalizer.fromProto[K, V](elem)),
      rightChild = protoInternal.rightChild.map(elem => NodeSerilalizer.fromProto[K, V](elem))
    )
  }

  def apply[K: Serializer: Monoid, V: Serializer: Monoid](key: K, value: V, height: Int, balance: Int)(
    implicit hash: Hashable[K]
  ): InternalNode[K, V] =
    new InternalNode(key, value, height, balance, None, None)
}
