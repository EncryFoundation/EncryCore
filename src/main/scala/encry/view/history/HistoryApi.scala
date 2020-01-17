package encry.view.history

import cats.syntax.option._
import encry.consensus.HistoryConsensus._
import encry.consensus._
import encry.modifiers.history._
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.ValidationError.HistoryApiError
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait HistoryApi extends HistoryDBApi { //scalastyle:ignore

  val timeProvider: NetworkTimeProvider



  final case class FastSyncProcessor(localSettings: EncryAppSettings) {
    var fastSyncVal: Boolean = settings.snapshotSettings.enableFastSynchronization && !settings.node.offlineGeneration
  }



  def toDownload(header: Header): Option[(ModifierTypeId, ModifierId)] =
    // Already synced and header is not too far back. Download required modifiers
    if (header.height >= blockDownloadProcessor.minimalBlockHeight) (Payload.modifierTypeId -> header.payloadId).some
    // Headers chain is synced after this header. Start downloading full blocks
    else if (!isHeadersChainSynced && isNewHeader(header)) {
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      none
    } else none

  def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier





}