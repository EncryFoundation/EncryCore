package encry.storage.levelDb.forksTree

import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.Algos

case class ForksTreeNode[D <: Diff](modifierId: ModifierId,
                                    diffs: Seq[D]) {

  def printTree: String = s"Node - ${Algos.encode(modifierId)}"
}

object ForksTreeNode {

  def apply[D <: Diff](modifierId: ModifierId,
                       diffs: Seq[D]): ForksTreeNode[D] =
    new ForksTreeNode[D](modifierId, diffs)

  def empty[D <: Diff]: ForksTreeNode[D] = ForksTreeNode(ModifierId @@ Array.emptyByteArray, Seq.empty[D])
}
