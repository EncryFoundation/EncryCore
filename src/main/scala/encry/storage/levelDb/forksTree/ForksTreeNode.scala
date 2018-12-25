package encry.storage.levelDb.forksTree

import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.Algos

case class ForksTreeNode[D <: Diff](var parent: Option[ForksTreeNode[D]],
                                    var children: Seq[ForksTreeNode[D]],
                                    modifierId: ModifierId,
                                    diffs: Seq[D]) {

  def addChildren(forksTreeNode: ForksTreeNode[D]): Unit = children = children :+ forksTreeNode

  def setParent(parentNode: ForksTreeNode[D]): Unit = parent = Some(parentNode)

  def printTree: String = s"Node - ${Algos.encode(modifierId)}. Children: ${children.map(_.printTree).mkString("\n")}"
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
