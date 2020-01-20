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

  var lastAvailableManifestHeight: Int = 0

  lazy val fastSyncInProgress: FastSyncProcessor = FastSyncProcessor(settings)

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else if (height > lastAvailableManifestHeight && fastSyncInProgress.fastSyncVal) acc
      else getBestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
        case Some(h) if !excluding.exists(_.sameElements(h.payloadId)) && !isBlockDefined(h) =>
          continuation(height + 1, acc :+ h.payloadId)
        case Some(_) =>
          continuation(height + 1, acc)
        case None =>
          acc
      }

    (for {
      bestBlockId             <- getBestBlockId
      headerLinkedToBestBlock <- getHeaderById(bestBlockId)
    } yield headerLinkedToBestBlock) match {
      case _ if !isHeadersChainSynced =>
        Seq.empty
      case Some(header) if isInBestChain(header) =>
        continuation(header.height + 1, Seq.empty)
      case Some(header) =>
        lastBestBlockHeightRelevantToBestChain(header.height)
          .map(height => continuation(height + 1, Seq.empty))
          .getOrElse(continuation(blockDownloadProcessor.minimalBlockHeightVar, Seq.empty))
      case None =>
        continuation(blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
    }
  }



}