package encry.view.state.avlTree

import io.iohk.iodb.ByteArrayWrapper

case class OperationInfo[K, V](insertedNodes: Map[ByteArrayWrapper, Node[K, V]] = Map.empty[ByteArrayWrapper, Node[K, V]],
                               deletedNodes: List[ByteArrayWrapper] = List.empty[ByteArrayWrapper]) {

  def update(newInserted: List[(ByteArrayWrapper, Node[K, V])] = List.empty[(ByteArrayWrapper, Node[K, V])],
             newDeleted: List[ByteArrayWrapper] = List.empty[ByteArrayWrapper]): OperationInfo[K, V] = {
    val toInsert = insertedNodes ++ newInserted
    val toDelete = deletedNodes.diff(toInsert.keys.toList)
    this.copy(
      toInsert,
      toDelete
    )
  }

  def update(newInserted: (ByteArrayWrapper, Node[K, V]),
             newDeleted: ByteArrayWrapper): OperationInfo[K, V] = {
    val toDelete = newDeleted +: deletedNodes
    val toInsert = (insertedNodes + newInserted) -- deletedNodes
    this.copy(
      toInsert,
      toDelete.diff(List(newInserted._1))
    )
  }

  def updateDeleted(newDeleted: ByteArrayWrapper): OperationInfo[K, V] =
    this.copy(insertedNodes - newDeleted, newDeleted +: deletedNodes)

  def updateInserted(newInserted: (ByteArrayWrapper, Node[K, V])): OperationInfo[K, V] = {
    val toInsert = insertedNodes + newInserted
    this.copy(toInsert, deletedNodes diff toInsert.keys.toList)
  }

  def updateInserted(newInserted: List[(ByteArrayWrapper, Node[K, V])]): OperationInfo[K, V] = {
    val toInsert = this.insertedNodes ++ newInserted
    this.copy(insertedNodes = toInsert)
  }
}

object OperationInfo {
  def empty[K, V]: OperationInfo[K, V] = OperationInfo[K, V]()
}
