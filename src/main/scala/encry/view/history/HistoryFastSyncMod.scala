package encry.view.history

import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait HistoryFastSyncMod extends HistoryApi {

  def payloadsIdsToDownloadFastSync(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
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