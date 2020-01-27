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

abstract class ShadowNode[K: Serializer: Monoid, V: Serializer: Monoid] extends Node[K, V] {
  def restoreFullNode(storage: VersionalStorage): Node[K, V]
  final override def selfInspection = this
}

object ShadowNode {

  def nodeToShadow[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): ShadowNode[K, V] = node match {
    case internal: InternalNode[K, V] =>
      NonEmptyShadowNode(internal.hash, internal.height, internal.balance, internal.key)
    case leaf: LeafNode[K, V] =>
      NonEmptyShadowNode(leaf.hash, leaf.height, leaf.balance, leaf.key)
    case _: EmptyNode[K, V] =>
      new EmptyShadowNode[K, V]
    case anotherShadow: ShadowNode[K, V] => anotherShadow
  }

  def childsToShadowNode[K: Serializer : Hashable : Monoid, V: Serializer : Monoid](node: Node[K, V]): Node[K, V] = node match {
    case internal: InternalNode[K, V] =>
      internal.copy(
        leftChild = nodeToShadow(internal.leftChild),
        rightChild = nodeToShadow(internal.rightChild),
      )
    case _ => node
  }
}
