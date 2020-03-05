package encry.view.history

import encry.consensus.HistoryConsensus.{ HistoryComparisonResult, Older }
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.HashSet

trait HistoryReader {

  def getBestHeaderHeight: Int

  def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId]

  def compare(si: SyncInfo): HistoryComparisonResult

  var isFullChainSynced: Boolean

  var isHeadersChainSyncedVar: Boolean = false

  def isModifierDefined(id: ModifierId): Boolean

  def modifierBytesById(id: ModifierId): Option[Array[Byte]]

  def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]

  def syncInfo: SyncInfo
}

object HistoryReader {
  def empty: HistoryReader = new HistoryReader {
    def isModifierDefined(id: ModifierId): Boolean                  = false
    def getBestHeaderHeight: Int                                    = -1
    def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = Seq.empty
    var isFullChainSynced: Boolean                                  = false
    def compare(si: SyncInfo): HistoryComparisonResult              = Older
    def modifierBytesById(id: ModifierId): Option[Array[Byte]]      = None
    def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]        = Seq.empty
    def syncInfo: SyncInfo                                          = SyncInfo(Seq.empty)
  }

  def apply(history: History): HistoryReader = new HistoryReader {
    def isModifierDefined(id: ModifierId): Boolean                  = history.isModifierDefined(id)
    def getBestHeaderHeight: Int                                    = history.getBestHeaderHeight
    def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] = history.continuationIds(info, size)
    def compare(si: SyncInfo): HistoryComparisonResult              = history.compare(si)
    var isFullChainSynced: Boolean                                  = history.isFullChainSynced
    def modifierBytesById(id: ModifierId): Option[Array[Byte]]      = history.modifierBytesById(id)
    def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]        = history.payloadsIdsToDownload(howMany, HashSet.empty)
    def syncInfo: SyncInfo                                          = history.syncInfo
  }
}
