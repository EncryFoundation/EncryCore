package encry.storage.levelDb.forksTree

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.NodeViewModifier
import encry.storage.levelDb.forksTree.ForksTree.BrunchNum
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.iq80.leveldb.DB
import scorex.crypto.hash.Digest32

import scala.util.Try

trait ForksTree[D <: RevertabaleDiff[D]] extends StrictLogging {

  val db: DB

  var diffsMap: Map[ModifierId, Seq[D]] = Map.empty

  var keysMap: Map[ModifierId, (Height, BrunchNum)] = Map.empty

  var modifiersTree: ForksTreeNode[D] = ForksTreeNode.empty[D]

  def add(modifier: NodeViewModifier): Unit

  def applyDiff(diff: D): Unit

  def getDiffsPath(targetNodeId: ModifierId,
                   currentNode: ForksTreeNode[D] = modifiersTree,
                   diffs: Seq[D] = Seq.empty,
                   persistantProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF]): Seq[D] = {
    if (targetNodeId == currentNode.modifierId) diffs
    else currentNode.parent
      .map(parentNode => getDiffsPath(
        targetNodeId,
        parentNode,
        diffs ++ currentNode.diffs.map(_.revert(persistantProver)),
        persistantProver)
      )
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
