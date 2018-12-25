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
    if (targetNodeId == currentNode.modifierId) {
      logger.info("targetNodeId == currentNode.modifierId")
      logger.info(s"Diffs: ${diffs}")
      diffs
    }
    else currentNode.children.flatMap(child => getDiffsPath(
        targetNodeId,
        child,
        diffs ++ currentNode.diffs.map(_.revert(persistantProver)),
        persistantProver)
      )
  }

  def rollbackTo(rollbackPoint: ModifierId,
                 prover: encry.avltree.PersistentBatchAVLProver[Digest32, HF]): Try[Unit] = Try {
    if (checkRollbackPoint(rollbackPoint)) {
      logger.info("Rollback point exists")
      val diffs = getDiffsPath(rollbackPoint, persistantProver = prover)
      logger.info(s"diifs: ${diffs}")
      if (modifiersTree.modifierId == rollbackPoint) {
        applyDiff(diffs.tail.foldLeft(diffs.head)(_ ++ _))
        logger.info(s"Successful rollback to: ${Algos.encode(rollbackPoint)}")
      }
      else modifiersTree.parent.map { parent =>
        modifiersTree = parent.copy(parent = None)
        rollbackTo(rollbackPoint, prover)
      }
    }
    else throw new Exception(s"Impossible to rollback to: ${Algos.encode(rollbackPoint)}")
  }

  private def checkRollbackPoint(rollbackPoint: ModifierId, node: ForksTreeNode[D] = modifiersTree): Boolean = {
    if (node.modifierId sameElements rollbackPoint) true
    else if (node.children.isEmpty) false
    else node.children.forall(child => checkRollbackPoint(rollbackPoint, child))
  }
}

object ForksTree {

  type BrunchNum = Long
}
