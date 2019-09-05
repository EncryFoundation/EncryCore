package encry.view.history

import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.ConsensusSchemeReaders
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import encry.settings.MainConstants.constants
import cats.syntax.option._
import scala.annotation.tailrec
import cats.syntax.either._

trait HistoryModifiersProcessors extends HistoryApi {

  def processHeader(h: Header): ProgressInfo = getHeaderInfoUpdate(h) match {
    case dataToUpdate: Seq[_] if dataToUpdate.nonEmpty =>
      historyStorage.bulkInsert(h.id, dataToUpdate, Seq(h))
      getBestHeaderId match {
        case Some(bestHeaderId) =>
          ProgressInfo(none, Seq.empty, if (!bestHeaderId.sameElements(h.id)) Seq.empty else Seq(h), toDownload(h))
        case _ =>
          forceStopApplication(errorMessage = "Should always have best header after header application")
      }
    case _ => ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  def processPayload(payload: Payload): ProgressInfo = getBlockByPayload(payload)
    .flatMap(block =>
      if (block.header.height - getBestBlockHeight >= 2 + settings.network.maxInvObjects) none
      else processBlock(block).some
    )
    .getOrElse(putToHistory(payload))

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
        logger.debug(s"Best full chain is empty. Returning empty progress info")
        ProgressInfo(none, Seq.empty, Seq.empty, none)
    }
  }

  private def processValidFirstBlock(fullBlock: Block,
                                     newBestHeader: Header,
                                     newBestChain: Seq[Block]): ProgressInfo = {
    logger.info(s"Appending ${fullBlock.encodedId} as a valid first block with height ${fullBlock.header.height}")
    updateStorage(fullBlock.payload, newBestHeader.id)
    ProgressInfo(none, Seq.empty, newBestChain, none)
  }

  private def processBetterChain(fullBlock: Block,
                                 newBestHeader: Header,
                                 newBestChain: Seq[Block],
                                 blocksToKeep: Int): ProgressInfo = getHeaderOfBestBlock.map { header =>
    val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(header, newBestHeader)
    val toRemove: Seq[Block] = prevChain
      .tail
      .headers
      .flatMap(getBlockByHeader)
    val toApply: Seq[Block] = newChain
      .tail
      .headers
      .flatMap(h => if (h == fullBlock.header) fullBlock.some else getBlockByHeader(h))
    toApply.foreach(addBlockToCacheIfNecessary)
    if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(fullBlock)
    else {
      //application of this block leads to full chain with higher score
      logger.info(s"Appending ${fullBlock.encodedId}|${fullBlock.header.height} as a better chain")
      val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
      val bestHeaderHeight: Int = getBestHeaderHeight
      val updateBestHeader: Boolean =
        (fullBlock.header.height > bestHeaderHeight) || (
          (fullBlock.header.height == bestHeaderHeight) &&
            scoreOf(fullBlock.id)
              .flatMap(fbScore => getBestHeaderId.flatMap(id => scoreOf(id).map(_ < fbScore)))
              .getOrElse(false)
          )
      updateStorage(fullBlock.payload, newBestHeader.id, updateBestHeader)
      if (blocksToKeep >= 0) {
        val lastKept: Int = blockDownloadProcessor.updateBestBlock(fullBlock.header)
        val bestHeight: Int = toApply.lastOption.map(_.header.height).getOrElse(0)
        val diff: Int = bestHeight - header.height
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

  def continuationHeaderChains(header: Header,
                               filterCond: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextHeightHeaders: Seq[Header] = headerIdsAtHeight(currentHeight + 1)
        .view
        .flatMap(getHeaderById)
        .filter(filterCond)
      if (nextHeightHeaders.isEmpty) acc.map(_.reverse)
      else {
        val updatedChains: Seq[Seq[Header]] = nextHeightHeaders.flatMap(h =>
          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
        )
        val nonUpdatedChains: Seq[Seq[Header]] =
          acc.filter(chain => !nextHeightHeaders.exists(_.parentId sameElements chain.head.id))

        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, Seq(Seq(header)))
  }

  private def getHeaderInfoUpdate(header: Header): Seq[(StorageKey, StorageValue)] = {
    addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Seq(
        BestHeaderKey                                -> StorageValue @@ header.id,
        heightIdsKey(constants.GenesisHeight) -> StorageValue @@ header.id,
        headerHeightKey(header.id)                   -> StorageValue @@ Ints.toByteArray(constants.GenesisHeight),
        headerScoreKey(header.id)                    -> StorageValue @@ header.difficulty.toByteArray
      )
    } else scoreOf(header.parentId).map { parentScore =>
      val score: Difficulty =
        Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
      val bestHeaderHeight: Int = getBestHeaderHeight
      val bestHeadersChainScore: BigInt = getBestHeadersChainScore
      val bestRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          Seq(BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId))
        else Seq.empty
      val scoreRow: (StorageKey, StorageValue) = headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
      val heightRow: (StorageKey, StorageValue) =
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
      val headerIdsRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          bestBlockHeaderIdsRow(header, score)
        else orphanedBlockHeaderIdsRow(header, score)
      Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow
    }.getOrElse(Seq.empty)
  }

  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New best header ${h.encodedId} with score: $score")
    val self: (StorageKey, StorageValue) =
      heightIdsKey(h.height) ->
        StorageValue @@ (Seq(h.id) ++ headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray
    val parentHeaderOpt: Option[Header] = getHeaderById(h.parentId)
    val forkHeaders: Seq[(StorageKey, StorageValue)] = parentHeaderOpt
      .toList
      .view
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filterNot(isInBestChain)
      .map(header =>
        heightIdsKey(header.height) ->
          StorageValue @@ (Seq(header.id) ++
            headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)).flatten.toArray
      )
      .toList
    forkHeaders :+ self
  }

  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> StorageValue @@ (headerIdsAtHeight(h.height) :+ h.id).flatten.toArray)
  }

  private def putToHistory(payload: Payload): ProgressInfo = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  private def isBetterChain(id: ModifierId): Boolean = (for {
    bestFullBlockId    <- getBestBlockId
    heightOfThisHeader <- getHeightByHeaderId(id)
    prevBestScore      <- scoreOf(bestFullBlockId)
    score              <- scoreOf(id)
    bestBlockHeight = getBestBlockHeight
  } yield (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore))
      .getOrElse(false)

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

  private def updateStorage(newModRow: PersistentModifier,
                            bestFullHeaderId: ModifierId,
                            updateHeaderInfo: Boolean = false): Unit = {
    val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
      else Seq(BestBlockKey -> bestFullHeaderId)
    historyStorage.bulkInsert(newModRow.id, indicesToInsert, Seq(newModRow))
  }

  private def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockId.isEmpty
}