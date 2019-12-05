package encry.view.state.avlTree.utils.implicits

import encry.view.state.avlTree.Node

trait Serializer[T] {

  def toBytes(elem: T): Array[Byte]
  def fromBytes(bytes: Array[Byte]): T
}
