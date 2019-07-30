package encry.view.history

import cats.syntax.option._
import cats.syntax.either._
import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.ConsensusSchemeReaders
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants

trait HistoryModifiersProcessor extends HistoryExtension {

  def process(modifier: PersistentModifier): ProgressInfo = modifier match {
    case header: Header =>
      logger.debug(s"Start processing header with id ${header.encodedId} at height ${header.height}")
      processHeader(header)
    case payload: Payload =>
      logger.debug(s"Start processing payload with id ${payload.encodedId} with header id ${Algos.encode(payload.headerId)}")
      processPayload(payload)
  }

  private def processHeader(h: Header): ProgressInfo = getHeaderInfoUpdate(h) match {
    case dataToUpdate: Seq[_] if dataToUpdate.nonEmpty =>
      logger.debug(s"While processing header, header info update is non empty. Inserting new data to db")
      history.bulkInsert(h.id, dataToUpdate, Seq(h)) //side effect
      getBestHeaderIdOpt match {
        case Some(bestHeaderId) => //todo do we need seq(h) toDownload
          logger.debug(s"New best header is in history. Updating ended successfully. New best header id is ${Algos.encode(bestHeaderId)}")
          ProgressInfo(none, Seq.empty, if (!bestHeaderId.sameElements(h.id)) Seq.empty else Seq(h), toDownload(h))
        case _ => forceStopApplication(errorMessage = "Should always has best header after header application")
      }
    case _ =>
      logger.debug(s"While processing header, header info update is empty. Returning empty progress info")
      ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  private def processPayload(payload: Payload): ProgressInfo = getHeaderById(payload.headerId)
    .flatMap(h =>
      if (h.height - getBestBlockHeight >= 2 + settings.network.maxInvObjects) none
      else processBlock(Block(h, payload)).some
    )
    .getOrElse(putToHistory(payload))

  private def processBlock(block: Block): ProgressInfo = {
    val bestFullChain: Seq[Block] = calculateBestFullChain(block)
    logger.debug(s"Start processing block with id ${block.encodedId} with height ${block.header.height}")
    //addBlockToCacheIfNecessary
    bestFullChain.lastOption.map(_.header) match {
      case Some(header) if isValidFirstBlock(block.header) =>
        logger.debug(s"Processing block is genesis")
        processValidFirstBlock(block, block.payload, header, bestFullChain, settings.node.blocksToKeep)
      case Some(header) if getBestBlockIdOpt.nonEmpty && isBetterChain(header.id) =>
        logger.debug(s"Processing block is from best chain")
        processBetterChain(block, block.payload, header, Seq.empty, settings.node.blocksToKeep)
      case Some(header) =>
        logger.debug(s"Processing block is from non best chain")
        processNonBestBlock(block, header, Seq.empty, settings.node.blocksToKeep)
      case None =>
        logger.debug(s"Best full chain is empty. Returning empty progress info")
        ProgressInfo(none, Seq.empty, Seq.empty, none) //todo new case
    }
  }

  private def processValidFirstBlock(fullBlock: Block,
                                     newModRow: PersistentModifier,
                                     newBestHeader: Header,
                                     newBestChain: Seq[Block],
                                     blocksToKeep: Int): ProgressInfo = {
    logger.info(s"Appending ${fullBlock.encodedId} as a valid first block")
    logStatus(toRemoveLength = 0, none, newBestChain.length, fullBlock.encodedId, fullBlock.payload.txs.size, none)
    updateStorage(newModRow, newBestHeader.id) //side effect
    ProgressInfo(none, Seq.empty, newBestChain, none)
  }

  private def processBetterChain(block: Block,
                                 newModRow: PersistentModifier,
                                 newBestHeader: Header,
                                 newBestChain: Seq[Block],
                                 blocksToKeep: Int): ProgressInfo = getHeaderOfBestBlock.map { header =>
    val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(header, newBestHeader)
      .getOrElse(HeaderChain.empty, HeaderChain.empty)
    val toRemove: Seq[Block] = prevChain
      .tail
      .headers
      .flatMap(h => getBlockById(h.id))
    val toApply: Seq[Block] = newChain
      .tail
      .headers
      .flatMap(h => if (h == block.header) block.some else getBlockById(h.id))
    //toApply.foreach(addBlockToCacheIfNecessary)
    if (toApply.lengthCompare(newChain.length - 1) != 0)
      processNonBestBlock(block, header, Seq.empty, settings.node.blocksToKeep)
    else {
      //application of this block leads to full chain with higher score
      logger.info(s"Appending block with id ${block.encodedId} and height ${block.header.height} as a better chain")
      logStatus(
        toRemove.size,
        toApply.lastOption.map(_.header),
        toApply.size,
        block.encodedId,
        block.payload.txs.size,
        header.some
      )
      val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
      val bestHeaderHeight: Int = getBestHeaderHeight
      val updateBestHeader: Boolean =
        (block.header.height > bestHeaderHeight) ||
          ((block.header.height == bestHeaderHeight) &&
            scoreOf(block.id)
              .flatMap(fbScore => getBestHeaderIdOpt.flatMap(id => scoreOf(id).map(_ < fbScore)))
              .getOrElse(false)
            )

      updateStorage(newModRow, newBestHeader.id, updateBestHeader) //side effect
      if (blocksToKeep >= 0) { //side effects //todo never used setting blocksToKeep
        val (lastKept: Int, newBlockDownloadProcessor: BlockDownloadProcessor) =
          blockDownloadProcessor.updateBestBlock(block.header.height)
        blockDownloadProcessor = newBlockDownloadProcessor
        val bestHeight: Int = toApply.lastOption.map(_.header.height).getOrElse(-1) //todo check getOrElse
        val diff: Int = bestHeight - header.height
        clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
      }
      ProgressInfo(branchPoint, toRemove, toApply, none)
    }
  }.getOrElse(ProgressInfo(none, Seq.empty, Seq.empty, none))

  private def processNonBestBlock(fullBlock: Block,
                                  newBestHeader: Header,
                                  newBestChain: Seq[Block],
                                  blocksToKeep: Int): ProgressInfo = {
    //Orphaned block or full chain is not initialized yet
    logStatus(toRemoveLength = 0, none, newBestChain.length, fullBlock.encodedId, fullBlock.payload.txs.size, none)
    history.bulkInsert(fullBlock.payload.id, Seq.empty, Seq(fullBlock.payload))
    ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  def getHeaderInfoUpdate(header: Header): Seq[(StorageKey, StorageValue)] = {
    //addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Seq(
        BestHeaderKey -> StorageValue @@ header.id,
        heightIdKey(TestNetConstants.GenesisHeight) -> StorageValue @@ header.id,
        modifierHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(TestNetConstants.GenesisHeight),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      )
    } else scoreOf(header.parentId).map { parentScore =>
      val score: Difficulty =
        Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
      val bestHeaderHeight: Int = getBestHeaderHeight
      val bestHeadersChainScore: BigInt = getBestHeadersChainScore.getOrElse(BigInt(0)) //todo check getOrElse
      val bestRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          Seq(BestHeaderKey -> StorageValue @@ header.id)
        else Seq.empty
      val scoreRow: (StorageKey, StorageValue) = headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
      val heightRow: (StorageKey, StorageValue) =
        modifierHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
      val headerIdsRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          bestBlockHeaderIdsRow(header, score)
        else orphanedBlockHeaderIdsRow(header, score)
      Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow
    }.getOrElse(Seq.empty)
  }

  /** Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New best header ${h.encodedId} with score $score and height ${h.height}")
    val self: (StorageKey, StorageValue) =
      heightIdKey(h.height) -> StorageValue @@ (h.id +: headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray //todo check :+
    val forkIds: Seq[(StorageKey, StorageValue)] = getHeaderById(h.parentId)
      .toSeq.view //todo check view
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers) //todo here we need only header.id and header.height
      .filterNot(isInBestChain)
      .map(h =>
        heightIdKey(h.height) ->
          StorageValue @@ (h.id +: headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray //todo check :+
      )
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdKey(h.height) -> StorageValue @@ (h.id +: headerIdsAtHeight(h.height)).flatten.toArray) //todo check :+
  }

  private def putToHistory(payload: Payload): ProgressInfo = {
    history.insertObjects(Seq(payload)) //side effect
    ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  private def clipBlockDataAt(heights: Seq[Int]): Either[Throwable, Unit] = Either.catchNonFatal {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(headerIdsAtHeight)
      .flatMap(getHeaderById)
      .map(h => h.payloadId)
    history.removeObjects(toRemove)
  }

  private def calculateBestFullChain(block: Block): Seq[Block] = {
    logger.debug(s"Starting calculateBestFullChain for block with id ${block.encodedId} and height ${block.header.height}")
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isModifierDefined(h.id))
      .map(_.drop(1))
    logger.debug(s"Continuations are ${continuations.map(seq => s"Seq contains: ${seq.length}").mkString(",")}")
    val chains: Seq[Seq[Block]] = continuations.map(_.flatMap(h => getBlockById(h.id))) //todo removed filter. Make attention
    logger.debug(s"Chains are ${chains.map(chain => s"chain contain: ${chain.length}").mkString(",")}")
    chains
      .map(c => block +: c) //todo don't understand this logic
      .maxBy(c => scoreOf(c.lastOption.map(_.id).getOrElse(ModifierId @@ Array.emptyByteArray)).getOrElse(BigInt(0))) //todo check height
  }

  def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockIdOpt.isEmpty

  private def isBetterChain(id: ModifierId): Boolean = (for {
    bestFullBlockId    <- getBestBlockIdOpt
    heightOfThisHeader <- getModifierHeightById(id) //todo possible combine with getHeader(id: ModifierId)
    prevBestScore      <- scoreOf(bestFullBlockId)
    score              <- scoreOf(id)
    bestBlockHeight = getBestBlockHeight
  } yield (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore))
    .getOrElse(false)

  private def updateStorage(newModRow: PersistentModifier,
                            bestFullHeaderId: ModifierId,
                            updateHeaderInfo: Boolean = false): Unit = {
    val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
      else Seq(BestBlockKey -> bestFullHeaderId)
    history.bulkInsert(newModRow.id, indicesToInsert, Seq(newModRow))
  }

  private def logStatus(toRemoveLength: Int,
                        toApplyLastHeader: Option[Header],
                        toApplyLength: Int,
                        appliedBlockEncodedId: String,
                        appliedBlockTxsLength: Int,
                        prevBest: Option[Header]): Unit = {
    val toRemoveStr: String = if (toRemoveLength == 0) "" else s" and to remove $toRemoveLength"
    val newStatusStr: String = if (toApplyLastHeader.isEmpty) "" else {
      s" New best block is ${toApplyLastHeader.map(_.encodedId)} " +
        s"with height ${toApplyLastHeader.map(_.height)} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${prevBest.map(_.height).getOrElse(-1)}"
    }
    logger.info(s"Full block $appliedBlockEncodedId appended (txs: $appliedBlockTxsLength), " +
      s"going to apply $toApplyLength, $toRemoveStr modifiers.$newStatusStr")
  }
}