package encry.view.history.tmp

import cats.syntax.option._
import cats.syntax.either._
import encry.consensus.HistoryConsensus
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.modifiers.history.HeaderChain
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId }

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait HistoryPayloadsFastSyncProcessorComponent extends HistoryPayloadsProcessorComponent {
  this: HistoryPrivateApi =>

  override val processor: PayloadProcessor = new FastSyncProcessor

  class FastSyncProcessor extends PayloadProcessor {

    var lastAvailableManifestHeight: Int = 0

    var fastSyncInProgress: Boolean =
      settings.snapshotSettings.enableFastSynchronization && !settings.node.offlineGeneration

    override def processPayload(payload: Payload): HistoryConsensus.ProgressInfo =
      getBlockByPayload(payload).flatMap { block =>
        processBlock(block).some
      }.getOrElse(putToHistory(payload))

    private def processBlock(blockToProcess: Block): ProgressInfo = {
      val bestFullChain: Seq[Block] = calculateBestFullChain(blockToProcess)
      addBlockToCacheIfNecessary(blockToProcess)
      bestFullChain.lastOption.map(_.header) match {
        case Some(header) if isValidFirstBlock(blockToProcess.header) =>
          processValidFirstBlock(blockToProcess, header, bestFullChain)
        case Some(header) if isBestBlockDefined && isBetterChain(header.id) =>
          processBetterChain(blockToProcess, header, Seq.empty, settings.node.blocksToKeep)
        case Some(_) =>
          nonBestBlock(blockToProcess)
        case None =>
          ProgressInfo(none, Seq.empty, Seq.empty, none)
      }
    }

    private def processValidFirstBlock(fullBlock: Block,
                                       newBestHeader: Header,
                                       newBestChain: Seq[Block]): ProgressInfo = {
      updateStorage(fullBlock.payload, newBestHeader.id)
      ProgressInfo(none, Seq.empty, newBestChain, none)
    }

    private def processBetterChain(fullBlock: Block,
                                   newBestHeader: Header,
                                   newBestChain: Seq[Block],
                                   blocksToKeep: Int): ProgressInfo =
      getHeaderOfBestBlock.map { header =>
        val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(header, newBestHeader)
        val toRemove: Seq[Block] = prevChain.tail.headers
          .flatMap(getBlockByHeader)
        val toApply: Seq[Block] = newChain.tail.headers
          .flatMap(h => if (h == fullBlock.header) fullBlock.some else getBlockByHeader(h))
        toApply.foreach(addBlockToCacheIfNecessary)
        if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(fullBlock)
        else {
          //application of this block leads to full chain with higher score
          val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
          val bestHeaderHeight: Int           = getBestHeaderHeight
          val updateBestHeader: Boolean =
            (fullBlock.header.height > bestHeaderHeight) || (
              (fullBlock.header.height == bestHeaderHeight) &&
                scoreOf(fullBlock.id)
                  .flatMap(fbScore => getBestHeaderId.flatMap(id => scoreOf(id).map(_ < fbScore)))
                  .getOrElse(false)
            )
          val updatedHeadersAtHeightIds =
            newChain.headers.map(header => updatedBestHeaderAtHeightRaw(header.id, Height @@ header.height)).toList
          updateStorage(fullBlock.payload, newBestHeader.id, updateBestHeader, updatedHeadersAtHeightIds)
          if (blocksToKeep >= 0) {
            val lastKept: Int   = blockDownloadProcessor.updateBestBlock(fullBlock.header)
            val bestHeight: Int = toApply.lastOption.map(_.header.height).getOrElse(0)
            val diff: Int       = bestHeight - header.height
            clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
          }
          ProgressInfo(branchPoint, toRemove, toApply, none)
        }
      }.getOrElse(ProgressInfo(none, Seq.empty, Seq.empty, none))

    private def nonBestBlock(fullBlock: Block): ProgressInfo = {
      //Orphaned block or full chain is not initialized yet
      historyStorage.bulkInsert(fullBlock.payload.id, Seq.empty, Seq(fullBlock.payload))
      ProgressInfo(none, Seq.empty, Seq.empty, none)
    }

    private def updatedBestHeaderAtHeightRaw(headerId: ModifierId, height: Height): (Array[Byte], Array[Byte]) =
      heightIdsKey(height) ->
        (Seq(headerId) ++
          headerIdsAtHeight(height).filterNot(_ sameElements headerId)).flatten.toArray

    private def putToHistory(payload: Payload): ProgressInfo = {
      historyStorage.insertObjects(Seq(payload))
      ProgressInfo(none, Seq.empty, Seq.empty, none)
    }

    private def isBetterChain(id: ModifierId): Boolean =
      (for {
        bestFullBlockId    <- getBestBlockId
        heightOfThisHeader <- getHeightByHeaderId(id)
        prevBestScore      <- scoreOf(bestFullBlockId)
        score              <- scoreOf(id)
        bestBlockHeight    = getBestBlockHeight
      } yield
        (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore))
        .getOrElse(false)

    private def calculateBestFullChain(block: Block): Seq[Block] = {
      val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isBlockDefined(h)).map(_.tail)
      val chains: Seq[Seq[Block]]         = continuations.map(_.filter(isBlockDefined).flatMap(getBlockByHeader))
      chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
    }

    private def clipBlockDataAt(heights: Seq[Int]): Either[Throwable, Unit] = Either.catchNonFatal {
      val toRemove: Seq[ModifierId] = heights
        .flatMap(headerIdsAtHeight)
        .flatMap(getHeaderById)
        .map(_.payloadId)
      historyStorage.removeObjects(toRemove)
    }

    private def updateStorage(newModRow: PersistentModifier,
                              bestFullHeaderId: ModifierId,
                              updateHeaderInfo: Boolean = false,
                              additionalIndexes: List[(Array[Byte], Array[Byte])] = List.empty): Unit = {
      val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
        if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
        else Seq(BestBlockKey                  -> bestFullHeaderId)
      historyStorage.bulkInsert(newModRow.id, indicesToInsert ++ additionalIndexes, Seq(newModRow))
    }

    private def isValidFirstBlock(header: Header): Boolean =
      header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockId.isEmpty

    override def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): List[ModifierId] = {
      @tailrec def continuation(height: Int, acc: List[ModifierId]): List[ModifierId] =
        if (acc.lengthCompare(howMany) >= 0) acc
        else if (height > lastAvailableManifestHeight && fastSyncInProgress) acc
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
          List.empty
        case Some(header) if isInBestChain(header) =>
          continuation(header.height + 1, List.empty)
        case Some(header) =>
          lastBestBlockHeightRelevantToBestChain(header.height)
            .map(height => continuation(height + 1, List.empty))
            .getOrElse(continuation(blockDownloadProcessor.minimalBlockHeightVar, List.empty))
        case None =>
          continuation(blockDownloadProcessor.minimalBlockHeightVar, List.empty)
      }
    }
  }

}
