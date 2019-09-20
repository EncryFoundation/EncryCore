package encry.view.state.avlTree

import encry.view.state.avlTree.utils.implicits.Hashable
import org.encryfoundation.common.utils.Algos

final case class LeafNode[K, V](key: K,
                                value: V)(implicit hashK: Hashable[K]) extends Node[K, V] {

  override val balance: Int = 0

  override val height: Int = 0

  override def selfInspection: Node[K, V] = this

  override val hash: Array[Byte] = hashK.hash(key)

  override def toString: String = s"($key, $value, height: 0, balance: 0, hash: ${Algos.encode(hash)})"

}
