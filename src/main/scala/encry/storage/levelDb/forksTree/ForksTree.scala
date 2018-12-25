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
//    logger.info(s"getDiffsPath for: ${Algos.encode(currentNode.modifierId)}")
    if (targetNodeId == currentNode.modifierId) {
//      logger.info("targetNodeId == currentNode.modifierId")
//      logger.info(s"Diffs: ${diffs}")
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
                 prover: encry.avltree.PersistentBatchAVLProver[Digest32, HF],
                 diffsPath: Seq[D] = Seq.empty[D]): Try[Unit] = Try {
    if (checkRollbackPoint(rollbackPoint)) {
      logger.info("Rollback point exists")
      logger.info(s"difs: ${diffsPath}")
      if (modifiersTree.modifierId == rollbackPoint) {
        logger.info("modifiersTree.modifierId == rollbackPoint")
        applyDiff(diffsPath.tail.foldLeft(diffsPath.head)(_ ++ _))
        //logger.info(s"Successful rollback to: ${Algos.encode(rollbackPoint)}")
      }
      else modifiersTree.children.map { child =>
        modifiersTree = child.copy(parent = None)
        logger.info(s"diffsPath.isEmpty: ${diffsPath.isEmpty}")
        rollbackTo(rollbackPoint, prover,
          //TODO: Remove if
          if (diffsPath.isEmpty) getDiffsPath(rollbackPoint, persistantProver = prover) else diffsPath)
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
