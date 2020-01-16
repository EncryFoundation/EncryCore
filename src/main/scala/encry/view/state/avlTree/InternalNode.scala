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
                                                                                      leftChild: Node[K, V],
                                                                                      rightChild: Node[K, V])
    extends Node[K, V] {

  override lazy val hash: Array[Byte] = {
    val serK = implicitly[Serializer[K]]
    val serV = implicitly[Serializer[V]]
    Algos.hash(
      Bytes.concat(serK.toBytes(key),
        serV.toBytes(value),
        Ints.toByteArray(height),
        Ints.toByteArray(balance),
        leftChild.hash,
        rightChild.hash
      )
    )
  }

  override def selfInspection: Node[K, V] = (leftChild.height, rightChild.height) match {
    case (-1, -1) => LeafNode(key, value)
    case (_, _) => this
  }

  def updateChilds(newLeftChild: Node[K, V] = leftChild,
                   newRightChild: Node[K, V] = rightChild): Node[K, V] = {
    val hashK = implicitly[Hashable[K]]
    val newLeftChildAfterInspect = newLeftChild.selfInspection
    val newRightChildAfterInspect = newRightChild.selfInspection
    this.copy(
      leftChild = newLeftChildAfterInspect,
      rightChild = newRightChildAfterInspect,
      balance = newLeftChildAfterInspect.height - newRightChildAfterInspect.height,
      height = Math.max(
        newLeftChildAfterInspect.height,
        if (newRightChildAfterInspect.height == -1) 0 else newRightChildAfterInspect.height
      ) + 1
    )
  }

  override def toString: String =
    s"[(${Algos.encode(implicitly[Serializer[K]].toBytes(key))}," +
      s" ${Algos.encode(implicitly[Serializer[V]].toBytes(value))}," +
      s" height: $height," +
      s" balance $balance, " +
      s" hash: ${Algos.encode(hash)}) \n-> LeftChildOf(${Algos.encode(implicitly[Serializer[K]].toBytes(key))}):" +
      s"${leftChild}, \n-> RightChildOf(${Algos.encode(implicitly[Serializer[K]].toBytes(key))}): ${rightChild}]"
}

object InternalNode {

  def toProto[K, V](node: InternalNode[K, V])(implicit kSer: Serializer[K], vSer: Serializer[V]): InternalNodeProto = {
    val msg = InternalNodeProto()
      .withBalance(node.balance)
      .withHeight(node.height)
      .withKey(ByteString.copyFrom(kSer.toBytes(node.key)))
      .withValue(ByteString.copyFrom(vSer.toBytes(node.value)))
    val withLeftChild = node.leftChild match {
      case _: EmptyNode[K, V] => msg
      case nonEmpty: Node[K, V] =>
        val child: NodeProtoMsg = NodeSerilalizer.toProto(nonEmpty)
        msg.withLeftChild(child)
    }
    node.rightChild match {
      case _: EmptyNode[K, V] => withLeftChild
      case nonEmpty: Node[K, V] =>
        val child: NodeProtoMsg = NodeSerilalizer.toProto(nonEmpty)
        withLeftChild.withRightChild(child)
    }
  }

  def fromProto[K: Monoid: Hashable, V: Monoid](
    protoInternal: InternalNodeProto
  )(implicit kSer: Serializer[K], vSer: Serializer[V]): Try[InternalNode[K, V]] = Try {
    new InternalNode(
      key = kSer.fromBytes(protoInternal.key.toByteArray),
      value = vSer.fromBytes(protoInternal.value.toByteArray),
      height = protoInternal.height,
      balance = protoInternal.balance,
      leftChild = protoInternal.leftChild.map(elem => NodeSerilalizer.fromProto[K, V](elem)).getOrElse(EmptyNode[K, V]),
      rightChild = protoInternal.rightChild.map(elem => NodeSerilalizer.fromProto[K, V](elem)).getOrElse(EmptyNode[K, V])
    )
  }

  def apply[K: Serializer: Monoid, V: Serializer: Monoid](key: K, value: V, height: Int, balance: Int)(
    implicit hash: Hashable[K]
  ): InternalNode[K, V] =
    new InternalNode(key, value, height, balance, EmptyNode[K, V], EmptyNode[K, V])
}
