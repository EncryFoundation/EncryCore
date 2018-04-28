package encry.view.history.processors

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

trait BlockProcessor extends BlockHeaderProcessor with ScorexLogging {

  import BlockProcessor._

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestBlockIdOpt: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  protected def getBlock(h: EncryBlockHeader): Option[EncryBlock]

  protected def commonBlockThenSuffixes(header1: EncryBlockHeader, header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain)

  protected[history] def continuationHeaderChains(header: EncryBlockHeader, withFilter: EncryBlockHeader => Boolean): Seq[Seq[EncryBlockHeader]]

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param payloadIsNew - flag, that transactions where added last
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processBlock(fullBlock: EncryBlock, payloadIsNew: Boolean): ProgressInfo[EncryPersistentModifier] = {
    val newModRow = calculateNewModRow(fullBlock, payloadIsNew)
    val bestFullChain = calculateBestFullChain(fullBlock.header)
    val newBestAfterThis = bestFullChain.last
    processing(ToProcess(fullBlock, newModRow, newBestAfterThis, nodeSettings.blocksToKeep))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  protected def isValidFirstBlock(header: EncryBlockHeader): Boolean = {
    header.height == blockDownloadProcessor.minimalBlockHeight && bestBlockIdOpt.isEmpty
  }

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestAfterThis, _)
      if isValidFirstBlock(fullBlock.header) =>

      logStatus(Seq(), Seq(fullBlock), fullBlock, None)
      updateStorage(newModRow, newBestAfterThis.id)
      ProgressInfo(None, Seq.empty, Seq(fullBlock), Seq.empty)
  }

  // FIXME: The problem seems to be here.
  private def processBetterChain: BlockProcessing = {
    case toProcess @ ToProcess(fullBlock, newModRow, newBestAfterThis, blocksToKeep)
      if bestBlockOpt.nonEmpty && isBetterChain(newBestAfterThis.id) =>

      val prevBest = bestBlockOpt.get
      val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, fullBlock.header)
      val toRemove: Seq[EncryBlock] = prevChain.tail.headers.flatMap(getBlock)
      val toApply: Seq[EncryBlock] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getBlock(h))

      if (toApply.lengthCompare(newChain.length - 1) != 0) {
        //block have higher score but is not linkable to full chain
        nonBestBlock(toProcess)
      } else {
        //application of this block leads to full chain with higher score
        logStatus(toRemove, toApply, fullBlock, Some(prevBest))
        val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

        updateStorage(newModRow, newBestAfterThis.id)

        if (blocksToKeep >= 0) {
          val lastKept = blockDownloadProcessor.updateBestBlock(fullBlock.header)
          val bestHeight: Int = newBestAfterThis.height
          val diff = bestHeight - prevBest.header.height
          clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  private def isBetterChain(id: ModifierId): Boolean = {
    val isBetter = for {
      bestFullBlockId <- bestBlockIdOpt
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(id)
      //TODO currentScore == prevBestScore
    } yield score > prevBestScore

    isBetter getOrElse false
  }

  private def nonBestBlock: BlockProcessing = { case params =>
    //Orphaned block or full chain is not initialized yet
    logStatus(Seq(), Seq(), params.fullBlock, None)
    historyStorage.bulkInsert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def calculateNewModRow(fullBlock: EncryBlock, txsAreNew: Boolean): EncryPersistentModifier = {
    if (txsAreNew) {
      fullBlock.payload
    } else {
      fullBlock.adProofsOpt
        .getOrElse(throw new NoSuchElementException("Only transactions can be new when proofs are empty"))
    }
  }

  private def calculateBestFullChain(header: EncryBlockHeader) = {
    val continuations = continuationHeaderChains(header, h => getBlock(h).nonEmpty).map(_.tail)
    val chains = continuations.map(hc => hc.map(getBlock).takeWhile(_.nonEmpty).flatten.map(_.header))
    chains.map(c => header +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  // Unused
  private def clipOnNewBestBlock(header: EncryBlockHeader): Unit = {
    heightOf(header.id).filter(h => h > nodeSettings.blocksToKeep)
      .foreach(h => clipBlockDataAt(Seq(h - nodeSettings.blocksToKeep)))
  }

  private def clipBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap { id => typedModifierById[EncryBlockHeader](id) }
      .flatMap { h =>
        Seq(h.adProofsId, h.payloadId)
      }
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(newModRow: EncryPersistentModifier,
                            bestFullHeaderId: ModifierId): Unit = {
    println("Best full header: " + Base58.encode(bestFullHeaderId))
    val indicesToInsert = Seq(BestBlockKey -> ByteArrayWrapper(bestFullHeaderId))
    historyStorage.bulkInsert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
      .ensuring(bestHeaderHeight >= bestBlockHeight, s"Headers height $bestHeaderHeight should be >= " +
        s"full height $bestBlockHeight")
  }

  private def storageVersion(newModRow: EncryPersistentModifier) = ByteArrayWrapper(newModRow.id)

  protected def modifierValidation(m: EncryPersistentModifier,
                                   headerOpt: Option[EncryBlockHeader]): Try[Unit] = {
    if (historyStorage.containsObject(m.id)) {
      Failure(new Error(s"Modifier $m is already in history"))
    } else {
      val minimalHeight = blockDownloadProcessor.minimalBlockHeight
      headerOpt match {
        case None =>
          Failure(new Error(s"Header for modifier $m is undefined"))
        case Some(header: EncryBlockHeader) if header.height < minimalHeight =>
          Failure(new Error(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight"))
        case Some(header: EncryBlockHeader) if !header.isRelated(m) =>
          Failure(new Error(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}"))
        case Some(_) =>
          Success()
      }
    }
  }

  private def logStatus(toRemove: Seq[EncryBlock],
                        toApply: Seq[EncryBlock],
                        appliedBlock: EncryBlock,
                        prevBest: Option[EncryBlock]): Unit = {
    val toRemoveStr = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr = if (toApply.isEmpty) "" else {
      s" New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${prevBest.map(_.header.height).getOrElse(-1)}"
    }
    log.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }
}

object BlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[EncryPersistentModifier]]

  case class ToProcess(fullBlock: EncryBlock,
                       newModRow: EncryPersistentModifier,
                       newBestAfterThis: EncryBlockHeader,
                       blocksToKeep: Int)
}
