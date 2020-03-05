package encry.view.history

import encry.consensus.HistoryConsensus.{HistoryComparisonResult, Older}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

trait HistoryReader {

  def getBestHeaderHeight: Int

  def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId]

  def compare(si: SyncInfo): HistoryComparisonResult

  var isFullChainSynced: Boolean

  var isHeadersChainSyncedVar: Boolean = false

  def isModifierDefined(id: ModifierId): Boolean

  def modifierBytesById(id: ModifierId): Option[Array[Byte]]

}

object HistoryReader {
  def empty: HistoryReader = new HistoryReader {
    def isModifierDefined(id: ModifierId): Boolean = true
    def getBestHeaderHeight = 0
    def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = Seq.empty
    var isFullChainSynced: Boolean = true
    def compare(si: SyncInfo): HistoryComparisonResult = Older
    def modifierBytesById(id: ModifierId): Option[Array[Byte]] = None
  }

  def apply(): HistoryReader = new HistoryReader {
    def isModifierDefined(id: ModifierId): Boolean = true
    def getBestHeaderHeight = 1
    def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = Seq.empty
    def compare(si: SyncInfo): HistoryComparisonResult = Older
    var isFullChainSynced: Boolean = true
    def modifierBytesById(id: ModifierId): Option[Array[Byte]] = None
  }
}