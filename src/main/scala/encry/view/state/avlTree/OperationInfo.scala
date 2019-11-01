package encry.view.state.avlTree

import com.typesafe.scalalogging.StrictLogging
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos

case class OperationInfo[K, V](insertedNodes: Map[ByteArrayWrapper, Node[K, V]] = Map.empty[ByteArrayWrapper, Node[K, V]],
                               deletedNodes: Set[ByteArrayWrapper] = Set.empty[ByteArrayWrapper]) extends StrictLogging {

  def resolve: (Array[(ByteArrayWrapper, Node[K, V])], List[ByteArrayWrapper]) = {
    val toDelete = deletedNodes.diff(insertedNodes.keys.toSet)
    val toInsert = insertedNodes.filterKeys(key => !deletedNodes.contains(key))
    toInsert.toArray -> toDelete.toList
  }

  def update(newInserted: List[(ByteArrayWrapper, Node[K, V])] = List.empty[(ByteArrayWrapper, Node[K, V])],
             newDeleted: List[ByteArrayWrapper] = List.empty[ByteArrayWrapper]): OperationInfo[K, V] = {
//    val toInsert = insertedNodes ++ newInserted
//    val toDelete = deletedNodes.diff(toInsert.keys.toList)
//    this.copy(
//      toInsert,
//      toDelete
//    )
    this.copy(
      insertedNodes ++ newInserted,
      (deletedNodes ++ newDeleted) -- newInserted.map(_._1)
    )
  }

  def update(newInserted: (ByteArrayWrapper, Node[K, V]),
             newDeleted: ByteArrayWrapper): OperationInfo[K, V] = {
//    val toDelete = newDeleted +: deletedNodes
//    val toInsert = (insertedNodes + newInserted) -- deletedNodes
//    this.copy(
//      toInsert,
//      toDelete.diff(List(newInserted._1))
//    )
    this.copy(
      insertedNodes + newInserted,
      (deletedNodes + newDeleted) - newInserted._1,
    )
  }

  def updateDeleted(newDeleted: ByteArrayWrapper): OperationInfo[K, V] = {
//    this.copy(insertedNodes - newDeleted, newDeleted +: deletedNodes)
    this.copy(insertedNodes = insertedNodes - newDeleted, deletedNodes = deletedNodes + newDeleted)
  }

  def updateInserted(newInserted: (ByteArrayWrapper, Node[K, V])): OperationInfo[K, V] = {
//    val toInsert = insertedNodes + newInserted
//    this.copy(toInsert, deletedNodes diff toInsert.keys.toList)
    this.copy(
      insertedNodes = insertedNodes + newInserted,
      deletedNodes = deletedNodes - newInserted._1
    )
  }

  def updateInserted(newInserted: List[(ByteArrayWrapper, Node[K, V])]): OperationInfo[K, V] = {
//    val toInsert = this.insertedNodes ++ newInserted
//    this.copy(insertedNodes = toInsert)
    this.copy(
      insertedNodes = insertedNodes ++ newInserted,
      deletedNodes = deletedNodes -- newInserted.map(_._1)
    )
  }
}

object OperationInfo {
  def empty[K, V]: OperationInfo[K, V] = OperationInfo[K, V]()
}
