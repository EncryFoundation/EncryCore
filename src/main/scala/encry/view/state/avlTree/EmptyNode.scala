package encry.view.state.avlTree

import NodeMsg.NodeProtoMsg.NodeTypes.EmptyNodeProto
import cats.kernel.Monoid
import encry.view.state.avlTree.AvlTree.Directions.EMPTY
import encry.view.state.avlTree.utils.implicits.Serializer
import io.iohk.iodb.ByteArrayWrapper

import scala.util.Try

final case class EmptyNode[K: Serializer: Monoid, V: Serializer: Monoid] private (key: K, value: V, height: Int, balance: Int) extends Node[K, V] {

  override def selfInspection = this

  override lazy val hash: Array[Byte] = Array.emptyByteArray
}

object EmptyNode {
  def apply[K, V]()(implicit k: Monoid[K],
                    v: Monoid[V],
                    kSer: Serializer[K],
                    vSer: Serializer[V]): EmptyNode[K, V] = new EmptyNode(k.empty, v.empty, height = -1, balance = 0)

  def toProto[K, V](node: EmptyNode[K, V]): EmptyNodeProto = EmptyNodeProto()

  def fromProto[K: Monoid : Serializer, V: Monoid : Serializer](protoNode: EmptyNodeProto): Try[EmptyNode[K, V]] = Try {EmptyNode[K, V]()}
}
