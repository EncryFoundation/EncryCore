package encry.view.state.avlTree.utils.implicits

import encry.view.state.avlTree.{Node, OperationInfo}

case class NodeWithOpInfo[K, V](node: Node[K, V], opInfo: OperationInfo[K, V] = OperationInfo.empty[K, V]) {

  def selfInspection: NodeWithOpInfo[K, V] = node.selfInspection(opInfo)
}
