package encry.view.history

import encry.consensus.HistoryConsensus.{ HistoryComparisonResult, Older }
import encry.modifiers.history.HeaderChain
import encry.view.history.ValidationError.HistoryApiError
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.{ Difficulty, ModifierId }

import scala.collection.immutable.HashSet

trait HistoryReader {

  def getBestHeaderId: Option[ModifierId]

  def getBestHeaderHeight: Int

  def getBestBlockHeight: Int

  def getBestHeaderAtHeight(h: Int): Option[Header]

  def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId]

  def compare(si: SyncInfo): HistoryComparisonResult

  def getHeaderById(id: ModifierId): Option[Header]

  def getChainToHeader(fromHeaderOpt: Option[Header], toHeader: Header): (Option[ModifierId], HeaderChain)

  def getBlockByHeaderId(id: ModifierId): Option[Block]

  def getBlockByHeader(header: Header): Option[Block]

  var isFullChainSynced: Boolean

  def isHeadersChainSynced: Boolean

  def isModifierDefined(id: ModifierId): Boolean

  def headerIdsAtHeight(height: Int): List[ModifierId]

  def modifierBytesById(id: ModifierId): Option[Array[Byte]]

  def payloadsIdsToDownload(howMany: Int): Seq[ModifierId]

  def lastHeaders(count: Int): HeaderChain

  def syncInfo: SyncInfo

  def isFastSyncInProcess: Boolean

  def getBestHeader: Option[Header]

  def getBestBlock: Option[Block]

  def testApplicable(modifier: PersistentModifier): Either[ValidationError, PersistentModifier]

  def requiredDifficultyAfter(parent: Header): Either[HistoryApiError, Difficulty]
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
    def getHeaderById(id: ModifierId): Option[Header]               = None
    def getBlockByHeaderId(id: ModifierId): Option[Block]           = None
    def getBlockByHeader(header: Header): Option[Block]             = None
    def headerIdsAtHeight(height: Int): List[ModifierId]            = List.empty[ModifierId]
    def lastHeaders(count: Int): HeaderChain                        = HeaderChain.empty
    def getBestHeader: Option[Header]                               = None
    def getBestBlock: Option[Block]                                 = None
    def getChainToHeader(
      fromHeaderOpt: Option[Header],
      toHeader: Header
    ): (Option[ModifierId], HeaderChain) =
      (None, HeaderChain.empty)
    def testApplicable(modifier: PersistentModifier): Either[ValidationError, PersistentModifier] =
      Left(HistoryApiError(""))
    def getBestHeaderId: Option[ModifierId]                                          = None
    def requiredDifficultyAfter(parent: Header): Either[HistoryApiError, Difficulty] = Left(HistoryApiError(""))
    def isHeadersChainSynced: Boolean = false
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
    def getHeaderById(id: ModifierId): Option[Header]               = history.getHeaderById(id)
    def headerIdsAtHeight(height: Int): List[ModifierId]            = history.headerIdsAtHeight(height).toList
    def getBlockByHeaderId(id: ModifierId): Option[Block]           = history.getBlockByHeaderId(id)
    def getBlockByHeader(header: Header): Option[Block]             = history.getBlockByHeader(header)
    def lastHeaders(count: Int): HeaderChain                        = history.lastHeaders(count)
    def getBestHeader: Option[Header]                               = history.getBestHeader
    def getBestBlock: Option[Block]                                 = history.getBestBlock
    def getChainToHeader(
      fromHeaderOpt: Option[Header],
      toHeader: Header
    ): (Option[ModifierId], HeaderChain) =
      history.getChainToHeader(fromHeaderOpt, toHeader)
    def testApplicable(modifier: PersistentModifier): Either[ValidationError, PersistentModifier] =
      history.testApplicable(modifier)
    def getBestHeaderId: Option[ModifierId] = history.getBestHeaderId
    def requiredDifficultyAfter(parent: Header): Either[HistoryApiError, Difficulty] =
      history.requiredDifficultyAfter(parent)
    def isHeadersChainSynced: Boolean = history.isHeadersChainSynced
  }
}
