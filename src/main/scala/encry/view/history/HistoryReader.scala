package encry.view.history

import encry.consensus.HistoryConsensus.{ HistoryComparisonResult, Older }
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.HashSet

trait HistoryReader {

  def getBestHeaderHeight: Int

  def getBestBlockHeight: Int

  def getBestHeaderAtHeight(h: Int): Option[Header]

  def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId]

  def compare(si: SyncInfo): HistoryComparisonResult

  var isFullChainSynced: Boolean

  var isHeadersChainSyncedVar: Boolean = false

  def isModifierDefined(id: ModifierId): Boolean

  def modifierBytesById(id: ModifierId): Option[Array[Byte]]

  def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]

  def syncInfo: SyncInfo

  def isFastSyncInProcess: Boolean
}

object HistoryReader {
  def empty: HistoryReader = new HistoryReader {
    def isModifierDefined(id: ModifierId): Boolean                  = false
    def getBestHeaderHeight: Int                                    = -1
    def getBestBlockHeight: Int                                     = -1
    def getBestHeaderAtHeight(h: Int): Option[Header]               = None
    def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = Seq.empty
    var isFullChainSynced: Boolean                                  = false
    def compare(si: SyncInfo): HistoryComparisonResult              = Older
    def modifierBytesById(id: ModifierId): Option[Array[Byte]]      = None
    def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]        = Seq.empty
    def syncInfo: SyncInfo                                          = SyncInfo(Seq.empty)
    def isFastSyncInProcess: Boolean                                = false
  }

  def apply(history: History): HistoryReader = new HistoryReader {
    def isModifierDefined(id: ModifierId): Boolean                  = history.isModifierDefined(id)
    def getBestHeaderHeight: Int                                    = history.getBestHeaderHeight
    def getBestBlockHeight: Int                                     = history.getBestBlockHeight
    def getBestHeaderAtHeight(h: Int): Option[Header]               = history.getBestHeaderAtHeight(h)
    def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = history.continuationIds(info, size)
    def compare(si: SyncInfo): HistoryComparisonResult              = history.compare(si)
    var isFullChainSynced: Boolean                                  = history.isFullChainSynced
    def modifierBytesById(id: ModifierId): Option[Array[Byte]]      = history.modifierBytesById(id)
    def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]        = history.payloadsIdsToDownload(howMany, HashSet.empty)
    def syncInfo: SyncInfo                                          = history.syncInfo
    def isFastSyncInProcess: Boolean                                = history.fastSyncInProgress.fastSyncVal
  }
}
