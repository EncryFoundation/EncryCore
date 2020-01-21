package encry.view.history.processors

import cats.syntax.option.none
import encry.consensus.HistoryConsensus
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait PayloadFastSyncProcessorComponent extends HistoryPayloadProcessorComponent {

  override val payloadProcessor = new PayloadFastSyncProcessor

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else if (height > lastAvailableManifestHeight && fastSyncInProgress.fastSyncVal) acc
      else
        getBestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
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

  class PayloadFastSyncProcessor extends PayloadProcessor {
    override def processPayload(payload: Payload): HistoryConsensus.ProgressInfo = {
      val startTime: Long = System.currentTimeMillis()
      getBlockByPayload(payload).foreach { block =>
        logger.info(s"processPayloadFastSync")
        historyStorage.bulkInsert(payload.id, Seq(BestBlockKey -> payload.headerId), Seq(payload))
        blockDownloadProcessor.updateBestBlock(block.header)
        logger.info(s"BlockDownloadProcessor updated block at height ${block.header.height} successfully")
        historyStorage.insert(
          StorageVersion @@ validityKey(block.payload.id).untag(StorageKey),
          List(block.header.id, block.payload.id).map(id => validityKey(id) -> StorageValue @@ Array(1.toByte))
        )
        logger.info(
          s"Finished processing block ${block.encodedId}. " +
            s"Processing time is ${(System.currentTimeMillis() - startTime) / 1000} s"
        )
      }
      ProgressInfo(none, Seq.empty, Seq.empty, none)
    }
  }
}
