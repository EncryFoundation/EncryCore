package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.NodeViewModifier
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDB.BrunchNum
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.iq80.leveldb.DB
import scorex.crypto.hash.Digest32
import scala.util.Try

trait VersionalLevelDB[D <: RevertabaleDiff[D]] extends StrictLogging {

  val db: DB

  var versionsList: List[Version[D]] = List.empty[Version[D]]

  def add(modifier: NodeViewModifier): Unit

  def applyDiff(diff: D): Unit

  def getAll: Seq[(Array[Byte], Array[Byte])] = {
    var elementsBuffer: Seq[(Array[Byte], Array[Byte])] = Seq.empty
    val iterator = db.iterator()
    iterator.seekToFirst()
    while (iterator.hasNext) {
      val nextElem = iterator.next()
      elementsBuffer = elementsBuffer :+ (nextElem.getKey -> nextElem.getValue)
    }
    iterator.seekToLast()
    elementsBuffer
  }

  def getDiffsPath(targetNodeId: ModifierId,
                   currentNodesList: List[Version[D]] = versionsList,
                   diffs: Seq[D] = Seq.empty,
                   persistantProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF]): Seq[D] = {
    logger.info(s"currentNodesList: ${currentNodesList.map(_.toString).mkString("\n")}")
    if (currentNodesList.nonEmpty && targetNodeId == currentNodesList.last.modifierId) diffs
    else if (currentNodesList.nonEmpty)
      getDiffsPath(
        targetNodeId,
        currentNodesList.init,
        diffs ++ currentNodesList.lastOption.map(_.diffs.map(_.revert(persistantProver))).getOrElse(Seq.empty),
        persistantProver
      )
    else diffs
  }

  def rollbackTo(rollbackPoint: ModifierId,
                 prover: encry.avltree.PersistentBatchAVLProver[Digest32, HF],
                 diffsPath: Seq[D] = Seq.empty[D]): Try[Unit] = Try {
    if (checkRollbackPoint(rollbackPoint)) {
      if (versionsList.last.modifierId == rollbackPoint) {
        applyDiff(diffsPath.tail.foldLeft(diffsPath.head)(_ ++ _))
      }
      else {
        val diffs = getDiffsPath(rollbackPoint, persistantProver = prover)
        if (versionsList.nonEmpty) {
          versionsList = versionsList.init
          rollbackTo(rollbackPoint, prover,
            //TODO: Remove if
            if (diffsPath.isEmpty) diffs else diffsPath)
        }
        else diffsPath
      }
    }
    else throw new Exception(s"Impossible to rollback to: ${Algos.encode(rollbackPoint)}")
  }

  private def checkRollbackPoint(rollbackPoint: ModifierId, nodesList: List[Version[D]] = versionsList): Boolean = {
    if (nodesList.last.modifierId sameElements rollbackPoint) true
    else if (nodesList.isEmpty) false
    else checkRollbackPoint(rollbackPoint, nodesList.init)
  }
}

object VersionalLevelDB {

  type BrunchNum = Long
}
