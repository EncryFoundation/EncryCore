package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg
import NodeMsg.NodeProtoMsg.NodeTypes
import cats.Monoid
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos

abstract class Node[K: Serializer: Monoid, V: Serializer: Monoid]{
  val key: K
  val value: V
  val height: Int
  val balance: Int
  def hash: Array[Byte] = Algos.hash(NodeSerilalizer.toBytes(this))
  def selfInspection: Node[K, V]

  override def hashCode(): Int = ByteArrayWrapper(hash).hashCode()
}


object NodeSerilalizer {
  def toProto[K: Serializer, V: Serializer](node: Node[K, V]): NodeProtoMsg = {
    val nodeSer = node match {
      case leaf: LeafNode[K, V]         => NodeTypes().withLeaf(LeafNode.toProto(leaf))
      case internal: InternalNode[K, V] => NodeTypes().withInternal(InternalNode.toProto(internal))
      case shadow: ShadowNode[K, V]     => NodeTypes().withShadow(ShadowNode.toProto(shadow))
      case empty: EmptyNode[K, V]       => NodeTypes().withEmptyNode(EmptyNode.toProto(empty))
    }
    NodeProtoMsg().withNode(nodeSer)
  }

  def fromProto[K: Serializer: Monoid: Hashable, V: Serializer: Monoid](nodeProto: NodeProtoMsg): Node[K, V] =
    nodeProto.node.get.nodeProto match {
      case NodeMsg.NodeProtoMsg.NodeTypes.NodeProto.Leaf(_) =>
        LeafNode.fromProto[K, V](nodeProto.node.get.nodeProto.leaf.get).get
      case NodeMsg.NodeProtoMsg.NodeTypes.NodeProto.Internal(_) =>
        InternalNode.fromProto[K, V](nodeProto.node.get.nodeProto.internal.get).get
      case NodeMsg.NodeProtoMsg.NodeTypes.NodeProto.Shadow(_) =>
        ShadowNode.fromProto[K, V](nodeProto.node.get.nodeProto.shadow.get).get
      case NodeMsg.NodeProtoMsg.NodeTypes.NodeProto.EmptyNode(_) =>
        EmptyNode.fromProto[K, V](nodeProto.node.get.nodeProto.emptyNode.get).get
    }

  def toBytes[K: Serializer: Monoid, V: Serializer: Monoid](node: Node[K, V]): Array[Byte] = toProto(node).toByteArray

  def fromBytes[K: Serializer: Monoid: Hashable, V: Serializer: Monoid](bytes: Array[Byte]): Node[K, V] =
    fromProto(NodeProtoMsg.parseFrom(bytes))
}
