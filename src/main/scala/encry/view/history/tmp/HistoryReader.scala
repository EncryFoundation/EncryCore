package encry.view.history.tmp

import encry.consensus.HistoryConsensus.HistoryComparisonResult
import encry.modifiers.history.HeaderChain
import encry.view.history.ValidationError.HistoryApiError
import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.{ Difficulty, ModifierId }

import scala.collection.immutable.HashSet

trait HistoryReader {

  def getHeaderById(id: ModifierId): Option[Header]

  def getBlockByHeader(header: Header): Option[Block]

  def getBestHeader: Option[Header]

  def getBlockById(id: ModifierId): Option[Block]

  def getBestHeaderHeight: Int

  def getBestBlock: Option[Block]

  def getBestBlockHeight: Int

  def getHeaderOfBestBlock: Option[Header]

  def getBestHeaderAtHeight(h: Int): Option[Header]

  def isBestBlockDefined: Boolean

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def modifierBytesById(id: ModifierId): Option[Array[Byte]]

  def lastHeaders(count: Int): HeaderChain

  def getHeaderIds(
    count: Int,
    offset: Int = 0
  ): Seq[ModifierId]

  def payloadsIdsToDownload(
    howMany: Int,
    excluding: HashSet[ModifierId]
  ): Seq[ModifierId]

  def requiredDifficultyAfter(parent: Header): Either[HistoryApiError, Difficulty]

  def syncInfo: SyncInfo

  def updateIdsForSyncInfo(): Unit

  def compare(si: SyncInfo): HistoryComparisonResult

  def continuationIds(
    info: SyncInfo,
    size: Int
  ): Seq[ModifierId]

  def getChainToHeader(
    fromHeaderOpt: Option[Header],
    toHeader: Header
  ): (Option[ModifierId], HeaderChain)

  def isHeadersChainSynced: Boolean
}
