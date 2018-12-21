package encry.storage.levelDb.forksTree

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.NodeViewModifier
import encry.storage.levelDb.forksTree.ForksTree.BrunchNum
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.History.Height
import org.encryfoundation.common.Algos

import scala.util.Try

trait ForksTree extends StrictLogging {

  var diffsMap: Map[ModifierId, Seq[Diff]] = Map.empty

  var keysMap: Map[ModifierId, (Height, BrunchNum)] = Map.empty

  var modifiersTree: ForksTreeNode = ForksTreeNode.empty

  def add(modifier: NodeViewModifier): Unit

  def getDiffsPath(targetNodeId: ModifierId,
                   currentNode: ForksTreeNode = modifiersTree,
                   diffs: Seq[Diff] = Seq.empty): Seq[Diff] = {
    if (targetNodeId == currentNode.modifierId) diffs
    else currentNode.parent
      .map(parentNode => getDiffsPath(targetNodeId, parentNode, diffs ++ currentNode.diffs.map(_.revert)))
      .getOrElse(diffs)
  }

  def rollbackTo(rollbackTarger: ModifierId): Try[Unit] = Try {
    if (modifiersTree.modifierId == rollbackTarger)
      logger.info(s"Successful rollback to: ${Algos.encode(rollbackTarger)}")
    else modifiersTree.parent.map{ parent =>
      modifiersTree = parent.copy(parent = None)
      rollbackTo(rollbackTarger)
    }.getOrElse(throw new Exception(s"Impossible to rollback to: ${Algos.encode(rollbackTarger)}"))
  }
}

object ForksTree {

  type BrunchNum = Long
}
