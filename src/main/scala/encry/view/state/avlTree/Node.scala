package encry.view.state.avlTree

import org.encryfoundation.common.serialization.BytesSerializable


trait Node[K, V] {
  val key: K
  val value: V
  val height: Int
  val balance: Int
  val hash: Array[Byte]
  def selfInspection: Node[K, V]
}
