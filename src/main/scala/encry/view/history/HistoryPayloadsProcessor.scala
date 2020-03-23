package encry.view.history

import cats.syntax.either._
import cats.syntax.option._
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.history.History.HistoryUpdateInfoAcc
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId }

trait HistoryPayloadsProcessor extends HistoryApi {

  def processPayload(payload: Payload): (ProgressInfo, Option[HistoryUpdateInfoAcc]) =
    getBlockByPayload(payload).flatMap { block =>
      logger.info(s"proc block ${block.header.encodedId}!")
      processBlock(block).some
    }.getOrElse(
      ProgressInfo(none, Seq.empty, Seq.empty, none) ->
        HistoryUpdateInfoAcc(Seq.empty, payload, insertToObjectStore = true).some
    )

  private def processBlock(blockToProcess: Block): (ProgressInfo, Option[HistoryUpdateInfoAcc]) = {
    logger.info(
      s"Starting processing block to history ||${blockToProcess.encodedId}||${blockToProcess.header.height}||"
    )
    val bestFullChain: Seq[Block] = calculateBestFullChain(blockToProcess)
    addBlockToCacheIfNecessary(blockToProcess)
    bestFullChain.lastOption.map(_.header) match {
      case Some(header) if isValidFirstBlock(blockToProcess.header) =>
        processValidFirstBlock(blockToProcess, header, bestFullChain)
      case Some(header) if isBestBlockDefined && isBetterChain(header.id) =>
        processBetterChain(blockToProcess, header, Seq.empty, settings.node.blocksToKeep)
      case Some(header) =>
        logger.info(
          s"\n\nnonBestBlock. id: ${blockToProcess.header.encodedId}. cause: ${isBestBlockDefined} or ${isBetterChain(header.id)}\n\n"
        )
        nonBestBlock(blockToProcess)
      case None =>
        logger.debug(s"Best full chain is empty. Returning empty progress info")
        ProgressInfo(none, Seq.empty, Seq.empty, none) -> none
    }
  }

  private def processValidFirstBlock(
    fullBlock: Block,
    newBestHeader: Header,
    newBestChain: Seq[Block]
  ): (ProgressInfo, Option[HistoryUpdateInfoAcc]) = {
    logger.info(s"Appending ${fullBlock.encodedId} as a valid first block with height ${fullBlock.header.height}")
    ProgressInfo(none, Seq.empty, newBestChain, none) -> Some(updateStorage(fullBlock.payload, newBestHeader.id))
  }

  private def processBetterChain(
    fullBlock: Block,
    newBestHeader: Header,
    newBestChain: Seq[Block],
    blocksToKeep: Int
  ): (ProgressInfo, Option[HistoryUpdateInfoAcc]) =
    getHeaderOfBestBlock.map { header =>
      val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(header, newBestHeader)
      val toRemove: Seq[Block] = prevChain.tail.headers
        .flatMap(getBlockByHeader)
      val toApply: Seq[Block] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) fullBlock.some else getBlockByHeader(h))
      toApply.foreach(addBlockToCacheIfNecessary)
      if (toApply.lengthCompare(newChain.length - 1) != 0) {
        logger.info(s"To apply. processBetterChain. nonBestBlock.")
        nonBestBlock(fullBlock)
      } else {
        //application of this block leads to full chain with higher score
        logger.info(s"Appending ${fullBlock.encodedId}|${fullBlock.header.height} as a better chain")
        val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
        val bestHeaderHeight: Int           = getBestHeaderHeight
        val updateBestHeader: Boolean =
          (fullBlock.header.height > bestHeaderHeight) || (
            (fullBlock.header.height == bestHeaderHeight) &&
              scoreOf(fullBlock.id)
                .flatMap(fbScore => getBestHeaderId.flatMap(scoreOf(_).map(_ < fbScore)))
                .getOrElse(false)
          )
        val updatedHeadersAtHeightIds =
          newChain.headers.map(header => updatedBestHeaderAtHeightRaw(header.id, Height @@ header.height)).toList
        val toUpdateInfo: HistoryUpdateInfoAcc =
          updateStorage(fullBlock.payload, newBestHeader.id, updateBestHeader, updatedHeadersAtHeightIds)
        if (blocksToKeep >= 0) {
          val lastKept: Int   = blockDownloadProcessor.updateBestBlock(fullBlock.header)
          val bestHeight: Int = toApply.lastOption.map(_.header.height).getOrElse(0)
          val diff: Int       = bestHeight - header.height
          clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, none) -> Some(toUpdateInfo)
      }
    }.getOrElse(ProgressInfo(none, Seq.empty, Seq.empty, none) -> None)

  private def nonBestBlock(fullBlock: Block): (ProgressInfo, Option[HistoryUpdateInfoAcc]) = {
    //Orphaned block or full chain is not initialized yet
    logger.info(s"Process block to history ${fullBlock.encodedId}||${fullBlock.header.height}||")
    ProgressInfo(none, Seq.empty, Seq.empty, none) ->
      HistoryUpdateInfoAcc(Seq.empty, fullBlock.payload, insertToObjectStore = false).some
  }

  private def updatedBestHeaderAtHeightRaw(headerId: ModifierId, height: Height): (Array[Byte], Array[Byte]) =
    heightIdsKey(height) ->
      (Seq(headerId) ++
        headerIdsAtHeight(height).filterNot(_ sameElements headerId)).flatten.toArray

  private def isBetterChain(id: ModifierId): Boolean =
    (for {
      bestFullBlockId    <- getBestBlockId
      heightOfThisHeader <- getHeightByHeaderId(id)
      prevBestScore      <- scoreOf(bestFullBlockId)
      score              <- scoreOf(id)
      bestBlockHeight    = getBestBlockHeight
    } yield {
      logger.info(
        s"isBetterChain. id: ${Algos.encode(id)}. \n " +
          s"bestBlockHeight: $bestBlockHeight.\n " +
          s"heightOfThisHeader $heightOfThisHeader.\n " +
          s"score: $score.\n " +
          s"prevBestScore: $prevBestScore.\n " +
          s"res is: ${(bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore)}"
      )
      (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore)
    }).getOrElse {
      logger.info(s"isBetterChain. id: ${Algos.encode(id)}. getOrElse. false.")
      false
    }

  private def calculateBestFullChain(block: Block): Seq[Block] = {
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isBlockDefined(h)).map(_.tail)
    logger.debug(s"continuations: ${continuations.map(seq => s"Seq contains: ${seq.length}").mkString(",")}")
    val chains: Seq[Seq[Block]] = continuations.map(_.filter(isBlockDefined).flatMap(getBlockByHeader))
    logger.debug(s"Chains: ${chains.map(chain => s"chain contain: ${chain.length}").mkString(",")}")
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  private def clipBlockDataAt(heights: Seq[Int]): Either[Throwable, Unit] = Either.catchNonFatal {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(headerIdsAtHeight)
      .flatMap(getHeaderById)
      .map(_.payloadId)
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(
    newModRow: PersistentModifier,
    bestFullHeaderId: ModifierId,
    updateHeaderInfo: Boolean = false,
    additionalIndexes: List[(Array[Byte], Array[Byte])] = List.empty
  ): HistoryUpdateInfoAcc = {
    val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
      else Seq(BestBlockKey                  -> bestFullHeaderId)
    HistoryUpdateInfoAcc((indicesToInsert ++ additionalIndexes).map {
      case (bytes, bytes1) =>
        StorageKey @@ bytes -> StorageValue @@ bytes1
    }, newModRow, insertToObjectStore = false)
  }

  private def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockId.isEmpty
}
