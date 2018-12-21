package encry.storage.levelDb.forksTree

import encry.utils.CoreTaggedTypes.ModifierId

case class ForksTreeNode(var parent: Option[ForksTreeNode],
                         var childrens: Seq[ForksTreeNode],
                         modifierId: ModifierId,
                         diffs: Seq[Diff]) {

  def addChildren(forksTreeNode: ForksTreeNode): Unit = childrens = childrens :+ forksTreeNode

  def setParent(parentNode: ForksTreeNode): Unit = parent = Some(parentNode)
}

object ForksTreeNode {
  val empty: ForksTreeNode = ForksTreeNode(None, Seq.empty, ModifierId @@ Array.emptyByteArray, Seq.empty)
}
