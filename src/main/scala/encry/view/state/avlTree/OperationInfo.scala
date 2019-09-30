package encry.view.state.avlTree

import io.iohk.iodb.ByteArrayWrapper

case class OperationInfo[K, V](insertedNodes: Map[ByteArrayWrapper, Node[K, V]] = Map.empty[ByteArrayWrapper, Node[K, V]],
                               deletedNodes: List[ByteArrayWrapper] = List.empty[ByteArrayWrapper]) {
  def update(newInserted: List[(ByteArrayWrapper, Node[K, V])] = List.empty[(ByteArrayWrapper, Node[K, V])],
             newDeleted: List[ByteArrayWrapper] = List.empty[ByteArrayWrapper]): OperationInfo[K, V] = {
    this.copy(
      insertedNodes ++ newInserted,
      deletedNodes ++ newDeleted
    )
  }

  def update(newInserted: (ByteArrayWrapper, Node[K, V]),
             newDeleted: ByteArrayWrapper): OperationInfo[K, V] = {
    this.copy(
      insertedNodes + newInserted,
      newDeleted +: deletedNodes
    )
  }

  def updateDeleted(newDeleted: ByteArrayWrapper): OperationInfo[K, V] = {
    this.copy(deletedNodes = newDeleted +: deletedNodes)
  }

  def updateInserted(newInserted: (ByteArrayWrapper, Node[K, V])): OperationInfo[K, V] = {
    this.copy(insertedNodes = insertedNodes + newInserted)
  }

  def updateInserted(newInserted: List[(ByteArrayWrapper, Node[K, V])]): OperationInfo[K, V] = {
    this.copy(insertedNodes = insertedNodes ++ newInserted)
  }
}

object OperationInfo {
  def empty[K, V]: OperationInfo[K, V] = OperationInfo[K, V]()
}
