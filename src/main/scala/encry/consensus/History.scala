package encry.consensus

import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.modifiers.PersistentNodeViewModifier
import org.encryfoundation.common.Algos

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
    * @tparam PM - type of used modifier
    */
  case class ProgressInfo[PM <: PersistentNodeViewModifier](branchPoint: Option[ModifierId],
                                                            toRemove: Seq[PM],
                                                            toApply: Seq[PM],
                                                            toDownload: Seq[(ModifierTypeId, ModifierId)]) {

    require(branchPoint.isDefined == toRemove.nonEmpty, s"Branch point should be defined for non-empty toRemove," +
      s" ${branchPoint.isDefined} == ${toRemove.nonEmpty} given")

    lazy val chainSwitchingNeeded: Boolean = toRemove.nonEmpty

    override def toString: String = s"ProgressInfo(BranchPoint: ${branchPoint.map(Algos.encode)}, " +
      s" to remove: ${toRemove.map(_.encodedId)}, to apply: ${toApply.map(_.encodedId)})"
  }

}