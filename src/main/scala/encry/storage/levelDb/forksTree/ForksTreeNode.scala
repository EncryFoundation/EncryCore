package encry.storage.levelDb.forksTree

import encry.utils.CoreTaggedTypes.ModifierId

case class ForksTreeNode[D <: Diff](var parent: Option[ForksTreeNode[D]],
                                    var childrens: Seq[ForksTreeNode[D]],
                                    modifierId: ModifierId,
                                    diffs: Seq[D]) {

  def addChildren(forksTreeNode: ForksTreeNode[D]): Unit = childrens = childrens :+ forksTreeNode

  def setParent(parentNode: ForksTreeNode[D]): Unit = parent = Some(parentNode)
}

object ForksTreeNode {

  def apply[D <: Diff](parent: ForksTreeNode[D],
                       childrens: Seq[ForksTreeNode[D]],
                       modifierId: ModifierId,
                       diffs: Seq[D]): ForksTreeNode[D] =
    new ForksTreeNode[D](Some(parent), childrens, modifierId, diffs)

  def apply[D <: Diff](childrens: Seq[ForksTreeNode[D]],
                       modifierId: ModifierId,
                       diffs: Seq[D]): ForksTreeNode[D] =
    new ForksTreeNode[D](None, childrens, modifierId, diffs)

  def empty[D <: Diff]: ForksTreeNode[D] = ForksTreeNode(None, Seq.empty, ModifierId @@ Array.emptyByteArray, Seq.empty)
}
