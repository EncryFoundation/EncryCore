package encry.view.history

import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.ConsensusSchemeReaders
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.history.processors.BlockProcessor.{BlockProcessing, ToProcess}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import supertagged.@@

import scala.annotation.tailrec
import scala.util.Try

trait HistoryModifiersProcessors extends HistoryExternalApi {

  protected def process(h: Header): ProgressInfo[PersistentModifier] = getHeaderInfoUpdate(h) match {
    case Some(dataToUpdate) =>
      historyStorage.bulkInsert(h.id, dataToUpdate._1, Seq(dataToUpdate._2))
      getBestHeaderId match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[Header] = if (!(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logger.error("Should always have best header after header application")
          forceStopApplication()
      }
    case None => ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def getHeaderInfoUpdate(header: Header): Option[(Seq[(StorageKey, StorageValue)], PersistentModifier)] = {
    addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Option(Seq(
        BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId),
        heightIdsKey(TestNetConstants.GenesisHeight) -> StorageValue @@ header.id.untag(ModifierId),
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(TestNetConstants.GenesisHeight),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      ) -> header)
    } else scoreOf(header.parentId).map { parentScore =>
      val score: BigInt @@ Difficulty.Tag =
        Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
      val bestRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > getBestHeaderHeight) ||
          (header.height == getBestHeaderHeight && score > getBestHeadersChainScore)) {
          Seq(BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId))
        } else Seq.empty
      val scoreRow: (StorageKey, StorageValue) =
        headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
      val heightRow: (StorageKey, StorageValue) =
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
      val headerIdsRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > getBestHeaderHeight) || (header.height == getBestHeaderHeight && score > getBestHeadersChainScore))
          bestBlockHeaderIdsRow(header, score)
        else orphanedBlockHeaderIdsRow(header, score)
      (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, header)
    }
  }


  /** Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    val prevHeight: Int = getBestHeaderHeight
    logger.info(s"New best header ${h.encodedId} with score: $score." +
      s" New height: ${h.height}, old height: $prevHeight")
    val self: (StorageKey, StorageValue) =
      heightIdsKey(h.height) ->
        StorageValue @@ (Seq(h.id) ++ headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray
    val parentHeaderOpt: Option[Header] = getHeaderById(h.parentId)
    val forkHeaders: Seq[Header] = parentHeaderOpt
      .toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(StorageKey, StorageValue)] = forkHeaders.map { header =>
      val otherIds: Seq[ModifierId] = headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)
      heightIdsKey(header.height) -> StorageValue @@ (Seq(header.id) ++ otherIds).flatten.toArray
    }
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  //todo why return type is Seq instead tuple2?
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> StorageValue @@ (headerIdsAtHeight(h.height) :+ h.id).flatten.toArray)
  }

  def process(payload: Payload): ProgressInfo[PersistentModifier] = getBlockByPayload(payload)
    .flatMap(block =>
      if (block.header.height - getBestBlockHeight >= 2 + settings.network.maxInvObjects) None
      else Some(processBlock(block, payload))
    )
    .getOrElse(putToHistory(payload))

  def getBlockByPayload(payload: Payload): Option[Block] = getHeaderById(payload.headerId)
    .flatMap(h => Some(Block(h, payload)))


  def putToHistory(payload: Payload): ProgressInfo[PersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  def processBlock(fullBlock: Block,
                   modToApply: PersistentModifier): ProgressInfo[PersistentModifier] = {
    val bestFullChain: Seq[Block] = calculateBestFullChain(fullBlock)
    logger.debug(s"best full chain contains: ${bestFullChain.length}")
    val newBestAfterThis: Header = bestFullChain.last.header
    addBlockToCacheIfNecessary(fullBlock)
    if (isValidFirstBlock(fullBlock.header))
      processValidFirstBlock(ToProcess(fullBlock, modToApply, newBestAfterThis, bestFullChain, settings.node.blocksToKeep))
    else if (getBestBlock.nonEmpty && isBetterChain(newBestAfterThis.id))
      processBetterChain(ToProcess(fullBlock, modToApply, newBestAfterThis, Seq.empty, settings.node.blocksToKeep))
    else nonBestBlock(ToProcess(fullBlock, modToApply, newBestAfterThis, Seq.empty, settings.node.blocksToKeep))
  }

  def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestHeader, newBestChain, _) if isValidFirstBlock(fullBlock.header) =>
      logger.info(s"Appending ${fullBlock.encodedId} as a valid first block")
      logStatus(Seq(), newBestChain, fullBlock, None)
      updateStorage(newModRow, newBestHeader.id)
      ProgressInfo(None, Seq.empty, newBestChain, Seq.empty)
  }

  //todo toRemove will be fixed later
  def processBetterChain: BlockProcessing = {
    case toProcess@ToProcess(fullBlock, newModRow, newBestHeader, _, blocksToKeep)
      if getBestBlock.nonEmpty && isBetterChain(newBestHeader.id) =>
      //todo remove .get
      val headerOfPrevBestBlock: Header = getHeaderOfBestBlock.get
      val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(headerOfPrevBestBlock, newBestHeader)
      val toRemove: Seq[Block] = prevChain.tail.headers.flatMap(getBlockByHeader)
      val toApply: Seq[Block] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getBlockByHeader(h))
      toApply.foreach(addBlockToCacheIfNecessary)
      if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(toProcess)
      else {
        //application of this block leads to full chain with higher score
        logger.info(s"Appending ${fullBlock.encodedId}|${fullBlock.header.height} as a better chain")
        logStatus(toRemove, toApply, fullBlock, Some(headerOfPrevBestBlock))
        val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
        val updateBestHeader: Boolean =
          (fullBlock.header.height > getBestHeaderHeight) || (
            (fullBlock.header.height == getBestHeaderHeight) &&
              scoreOf(fullBlock.id)
                .flatMap(fbScore => getBestHeaderId.flatMap(id => scoreOf(id).map(_ < fbScore)))
                .getOrElse(false))

        updateStorage(newModRow, newBestHeader.id, updateBestHeader)
        if (blocksToKeep >= 0) {
          val lastKept: Int = blockDownloadProcessor.updateBestBlock(fullBlock.header)
          val bestHeight: Int = toApply.last.header.height
          val diff: Int = bestHeight - headerOfPrevBestBlock.height
          clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockId.isEmpty

  def isBetterChain(id: ModifierId): Boolean = {
    val isBetter: Option[Boolean] = for {
      bestFullBlockId <- getBestBlockId
      //todo possible combine with getHeader(id: ModifierId)
      heightOfThisHeader <- headersCache.get(ByteArrayWrapper(id)).map(_.height).orElse(getHeightByHeaderId(id))
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(id)
    } yield (getBestBlockHeight < heightOfThisHeader) || (getBestBlockHeight == heightOfThisHeader && score > prevBestScore)
    isBetter getOrElse false
  }


  def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq.empty, Seq.empty, params.fullBlock, None)
      historyStorage.bulkInsert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }


  def calculateBestFullChain(block: Block): Seq[Block] = {
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isBlockDefined(h)).map(_.tail)
    logger.debug(s"continuations: ${continuations.map(seq => s"Seq contains: ${seq.length}").mkString(",")}")
    val chains: Seq[Seq[Block]] = continuations.map(_.filter(isBlockDefined).flatMap(getBlockByHeader))
    logger.debug(s"Chains: ${chains.map(chain => s"chain contain: ${chain.length}").mkString(",")}")
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  def clipBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(h => headerIdsAtHeight(h))
      .flatMap(getHeaderById)
      .map(h => h.payloadId)
    historyStorage.removeObjects(toRemove)
  }

  def updateStorage(newModRow: PersistentModifier,
                    bestFullHeaderId: ModifierId,
                    updateHeaderInfo: Boolean = false): Unit = {
    val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
      else Seq(BestBlockKey -> bestFullHeaderId)
    historyStorage.bulkInsert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
  }

  def storageVersion(newModRow: PersistentModifier): ModifierId = newModRow.id


  def logStatus(toRemove: Seq[Block],
                toApply: Seq[Block],
                appliedBlock: Block,
                prevBest: Option[Header]): Unit = {
    val toRemoveStr: String = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr: String = if (toApply.isEmpty) "" else {
      s" New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${prevBest.map(_.height).getOrElse(-1)}"
    }
    logger.info(s"Full block ${appliedBlock.encodedId} appended (txs: ${appliedBlock.payload.txs.length}), " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }

  protected[history] def continuationHeaderChains(header: Header,
                                                  filterCond: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec
    def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextLevelHeaders: Seq[Header] = Seq(currentHeight)
        .flatMap(h => headerIdsAtHeight(h + 1))
        .flatMap(getHeaderById)
        .filter(filterCond)
      if (nextLevelHeaders.isEmpty) acc.map(_.reverse)
      else {
        val updatedChains: Seq[Seq[Header]] = nextLevelHeaders.flatMap { h =>
          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
        }
        val nonUpdatedChains: Seq[Seq[Header]] = acc.filter(chain =>
          !nextLevelHeaders.exists(_.parentId sameElements chain.head.id))
        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, Seq(Seq(header)))
  }
}
