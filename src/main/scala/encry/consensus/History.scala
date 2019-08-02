package encry.consensus

import org.encryfoundation.common.modifiers.{PersistentModifier, PersistentNodeViewModifier}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

object History {

  type ModifierIds = Seq[(ModifierTypeId, ModifierId)]

  sealed trait HistoryComparisonResult

  case object Equal extends HistoryComparisonResult

  case object Younger extends HistoryComparisonResult

  case object Older extends HistoryComparisonResult

  case object Fork extends HistoryComparisonResult

  case object Unknown extends HistoryComparisonResult

  /**
    * Info returned by history to nodeViewHolder after modifier application
    *
    * @param branchPoint - branch point in case of rollback
    * @param toRemove    - modifiers to remove from current node view
    * @param toApply     - modifiers to apply to current node view
    * @param toDownload  - modifiers to download from other nodes
    */
  case class ProgressInfo(branchPoint: Option[ModifierId],
                          toRemove: Seq[PersistentModifier],
                          toApply: Seq[PersistentModifier],
                          toDownload: Seq[(ModifierTypeId, ModifierId)]) {

    require(branchPoint.isDefined == toRemove.nonEmpty, s"Branch point should be defined for non-empty toRemove," +
      s" ${branchPoint.isDefined} == ${toRemove.nonEmpty} given")

    lazy val chainSwitchingNeeded: Boolean = toRemove.nonEmpty

    override def toString: String = s"ProgressInfo(BranchPoint: ${branchPoint.map(Algos.encode)}, " +
      s" to remove: ${toRemove.map(_.encodedId)}, to apply: ${toApply.map(_.encodedId)})"
  }

}