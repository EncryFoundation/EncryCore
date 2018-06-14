package encry.consensus

import encry.consensus.History.ProgressInfo
import scorex.core._
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * History of a blockchain system is some blocktree, where longest chain is
  * being considered as canonical one, containing right kind of history.
  *
  * In cryptocurrencies of today blocktree view is usually implicit, means code supports only linear history,
  * but other options are possible.
  *
  * To say "longest chain" is the canonical one is simplification, usually some kind of "cumulative difficulty"
  * function has been used instead.
  */
trait History[PM <: PersistentNodeViewModifier, SI <: SyncInfo, HT <: History[PM, SI, HT]] extends HistoryReader[PM, SI] {

  /**
    * @return append modifier to history
    */
  def append(modifier: PM): Try[(HT, ProgressInfo[PM])]

  /**
    * Report that modifier is valid from point of view of the state component
    * @param modifier - valid modifier
    * @return modified history
    */
  def reportModifierIsValid(modifier: PM): HT

  /**
    * Report that modifier is invalid from other nodeViewHolder components point of view
    * @param modifier - invalid modifier
    * @param progressInfo - what suffix failed to be applied because of an invalid modifier
    * @return modified history and new progress info
    */
  def reportModifierIsInvalid(modifier: PM, progressInfo: ProgressInfo[PM]): (HT, ProgressInfo[PM])


  /**
    * @return read-only copy of this history
    */
  def getReader: HistoryReader[PM, SI] = this

}

object History {

  type ModifierIds = Seq[(ModifierTypeId, ModifierId)]

  sealed trait HistoryComparisonResult
  case object Equal extends HistoryComparisonResult
  case object Younger extends HistoryComparisonResult
  case object Older extends HistoryComparisonResult
  case object Nonsense extends HistoryComparisonResult
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
                                                            toDownload: Seq[(ModifierTypeId, ModifierId)]
                                                           ) {

    require(branchPoint.isDefined == toRemove.nonEmpty, s"Branch point should be defined for non-empty toRemove," +
      s" ${branchPoint.isDefined} == ${toRemove.nonEmpty} given")

    lazy val chainSwitchingNeeded: Boolean = toRemove.nonEmpty

    override def toString: String = {
      s"ProgressInfo(BranchPoint: ${branchPoint.map(Base58.encode)}, " +
        s" to remove: ${toRemove.map(_.encodedId)}, to apply: ${toApply.map(_.encodedId)})"
    }
  }

}