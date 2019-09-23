package encry.view.state.avlTree

import Node.NodeProtoMsg.NodeTypes.EmptyNodeProto
import cats.kernel.Monoid
import encry.view.state.avlTree.AvlTree.Direction
import encry.view.state.avlTree.AvlTree.Directions.EMPTY

import scala.util.Try

final case class EmptyNode[K, V] private (key: K, value: V, height: Int, balance: Int) extends Node[K, V] {

  override def selfInspection: Node[K, V] = this

  override val hash: Array[Byte] = Array.emptyByteArray
}

object EmptyNode {
  def apply[K, V]()(implicit k: Monoid[K],
                    v: Monoid[V]): EmptyNode[K, V] = new EmptyNode(k.empty, v.empty, height = 0, balance = 0)

  def toProto[K, V](node: EmptyNode[K, V]): EmptyNodeProto = EmptyNodeProto()

  def fromProto[K: Monoid, V: Monoid](protoNode: EmptyNodeProto): Try[EmptyNode[K, V]] = Try {EmptyNode[K, V]()}
}
