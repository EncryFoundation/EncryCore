package encry.view.history

import cats.syntax.option._
import encry.EncryApp.forceStopApplication
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.settings.EncryAppSettings
import encry.view.history.processors.BlockProcessor.{BlockProcessing, ToProcess}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

trait HistoryModifiersProcessor extends HistoryExtension {

  val settings: EncryAppSettings

  def process(mod: PersistentModifier): ProgressInfo[PersistentModifier] = mod match {
    case h: Header => processHeader(h)
    case p: Payload => processPayload(p)
  }

  private def processHeader(h: Header): ProgressInfo[PersistentModifier] = getHeaderInfoUpdate(h) match {
    case dataToUpdate: Seq[_] if dataToUpdate.nonEmpty =>
      history.bulkInsert(h.id, dataToUpdate, Seq(h)) //side effect
      getBestHeaderIdOpt match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[Header] = if (!(bestHeaderId sameElements h.id)) Seq.empty else Seq(h) //todo do we need seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logger.error("Should always have best header after header application")
          forceStopApplication() //todo possibly remove this
      }
    case _ => ProgressInfo(None, Seq.empty, Seq.empty, none)
  }

  private def processPayload(payload: Payload): ProgressInfo[PersistentModifier] = getHeaderById(payload.headerId)
    .flatMap(h =>
      if (h.height - getBestBlockHeight >= 2 + settings.network.maxInvObjects) none
      else processBlock(Block(h, payload), payload).some
    ).getOrElse(putToHistory(payload))

  private def processBlock(block: Block, modToApply: PersistentModifier): ProgressInfo[PersistentModifier] = {
    val bestFullChain: Seq[Block] = calculateBestFullChain(block)
    val newBestAfterThis: Header = bestFullChain.last.header
    //addBlockToCacheIfNecessary(fullBlock)
    if (isValidFirstBlock(block.header))
      processValidFirstBlock(ToProcess(block, modToApply, newBestAfterThis, bestFullChain, settings.node.blocksToKeep))
    else if (getBestBlockIdOpt.nonEmpty && isBetterChain(newBestAfterThis.id))
      processBetterChain(ToProcess(block, modToApply, newBestAfterThis, Seq.empty, settings.node.blocksToKeep))
    else nonBestBlock(ToProcess(block, modToApply, newBestAfterThis, Seq.empty, settings.node.blocksToKeep))
  }

  private def putToHistory(payload: Payload): ProgressInfo[PersistentModifier] = {
    history.insertObjects(Seq(payload)) //side effect
    ProgressInfo(None, Seq.empty, Seq.empty, none)
  }

  private def calculateBestFullChain(block: Block): Seq[Block] = {
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isBlockDefined(h)).map(_.tail)
    logger.debug(s"continuations: ${continuations.map(seq => s"Seq contains: ${seq.length}").mkString(",")}")
    val chains: Seq[Seq[Block]] = continuations.map(_.filter(isBlockDefined).flatMap(getBlock))
    logger.debug(s"Chains: ${chains.map(chain => s"chain contain: ${chain.length}").mkString(",")}")
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && bestBlockIdOpt.isEmpty

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestHeader, newBestChain, _) if isValidFirstBlock(fullBlock.header) =>
      logger.info(s"Appending ${fullBlock.encodedId} as a valid first block")
      logStatus(Seq(), newBestChain, fullBlock, None)
      updateStorage(newModRow, newBestHeader.id)
      ProgressInfo(None, Seq.empty, newBestChain, Seq.empty)
  }

  //todo toRemove will be fixed later
  private def processBetterChain: BlockProcessing = {
    case toProcess@ToProcess(fullBlock, newModRow, newBestHeader, _, blocksToKeep)
      if bestBlockOpt.nonEmpty && isBetterChain(newBestHeader.id) =>
      //todo remove .get
      val headerOfPrevBestBlock: Header = headerOfBestBlock.get
      val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(headerOfPrevBestBlock, newBestHeader)
      val toRemove: Seq[Block] = prevChain.tail.headers.flatMap(getBlock)
      val toApply: Seq[Block] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getBlock(h))
      toApply.foreach(addBlockToCacheIfNecessary)
      if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(toProcess)
      else {
        //application of this block leads to full chain with higher score
        logger.info(s"Appending ${fullBlock.encodedId}|${fullBlock.header.height} as a better chain")
        logStatus(toRemove, toApply, fullBlock, Some(headerOfPrevBestBlock))
        val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
        val updateBestHeader: Boolean =
          (fullBlock.header.height > bestHeaderHeight) || (
            (fullBlock.header.height == bestHeaderHeight) &&
              scoreOf(fullBlock.id)
                .flatMap(fbScore => bestHeaderIdOpt.flatMap(id => scoreOf(id).map(_ < fbScore)))
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

  private def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq.empty, Seq.empty, params.fullBlock, None)
      historyStorage.bulkInsert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}