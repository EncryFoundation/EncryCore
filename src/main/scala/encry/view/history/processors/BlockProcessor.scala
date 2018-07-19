package encry.view.history.processors

import encry.ModifierId
import encry.consensus.History.ProgressInfo
import encry.consensus.ModifierSemanticValidity.Invalid
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.utils.Logging
import encry.validation.{ModifierValidator, RecoverableModifierError, ValidationResult}
import io.iohk.iodb.ByteArrayWrapper
import scala.util.{Failure, Try}

trait BlockProcessor extends BlockHeaderProcessor with Logging {

  import BlockProcessor._

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestBlockIdOpt: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  protected def getBlock(h: EncryBlockHeader): Option[EncryBlock]

  protected def commonBlockThenSuffixes(header1: EncryBlockHeader, header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain)

  protected[history] def continuationHeaderChains(header: EncryBlockHeader, filterCond: EncryBlockHeader => Boolean): Seq[Seq[EncryBlockHeader]]

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param modToApply - new part of the block we want to apply
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processBlock(fullBlock: EncryBlock, modToApply: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] = {
    val bestFullChain: Seq[EncryBlock] = calculateBestFullChain(fullBlock)
    val newBestAfterThis: EncryBlockHeader = bestFullChain.last.header
    processing(ToProcess(fullBlock, modToApply, newBestAfterThis, bestFullChain, nodeSettings.blocksToKeep))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestHeader, newBestChain, _)
      if isValidFirstBlock(fullBlock.header) =>
      logStatus(Seq(), newBestChain, fullBlock, None)
      updateStorage(newModRow, newBestHeader.id)
      ProgressInfo(None, Seq.empty, newBestChain, Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case toProcess @ ToProcess(fullBlock, newModRow, newBestHeader, _, blocksToKeep)
      if bestBlockOpt.nonEmpty && isBetterChain(newBestHeader.id) =>

      val prevBest: EncryBlock = bestBlockOpt.get
      val (prevChain: EncryHeaderChain, newChain: EncryHeaderChain) = commonBlockThenSuffixes(prevBest.header, newBestHeader)
      val toRemove: Seq[EncryBlock] = prevChain.tail.headers.flatMap(getBlock)
      val toApply: Seq[EncryBlock] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getBlock(h))

      if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(toProcess) //block have higher score but is not linkable to full chain
      else {
        //application of this block leads to full chain with higher score
        logStatus(toRemove, toApply, fullBlock, Some(prevBest))
        val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)

        updateStorage(newModRow, newBestHeader.id)

        if (blocksToKeep >= 0) {
          val lastKept: Int = blockDownloadProcessor.updateBestBlock(fullBlock.header)
          val bestHeight: Int = toApply.last.header.height
          val diff: Int = bestHeight - prevBest.header.height
          clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  protected def isValidFirstBlock(header: EncryBlockHeader): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && bestBlockIdOpt.isEmpty

  private def isBetterChain(id: ModifierId): Boolean = {
    val isBetter: Option[Boolean] = for {
      bestFullBlockId <- bestBlockIdOpt
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(id)
      //TODO currentScore == prevBestScore
    } yield score > prevBestScore

    isBetter getOrElse false
  }

  private def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq(), Seq(), params.fullBlock, None)
      historyStorage.bulkInsert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def calculateNewModRow(fullBlock: EncryBlock, txsAreNew: Boolean): EncryPersistentModifier =
    if (txsAreNew) fullBlock.payload
    else fullBlock.adProofsOpt.getOrElse(throw new NoSuchElementException("Only transactions can be new when proofs are empty"))

  private def calculateBestFullChain(block: EncryBlock): Seq[EncryBlock] = {
    val continuations: Seq[Seq[EncryBlockHeader]] = continuationHeaderChains(block.header, h => getBlock(h).nonEmpty).map(_.tail)
    val chains: Seq[Seq[EncryBlock]] = continuations.map(hc => hc.map(getBlock).takeWhile(_.nonEmpty).flatten)
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  private def clipBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[EncryBlockHeader](id))
      .flatMap(h => Seq(h.adProofsId, h.payloadId))
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(newModRow: EncryPersistentModifier, bestFullHeaderId: ModifierId): Unit = {
    val indicesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(BestBlockKey -> ByteArrayWrapper(bestFullHeaderId))
    historyStorage.bulkInsert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
      .ensuring(bestHeaderHeight >= bestBlockHeight, s"Headers height $bestHeaderHeight should be >= " +
        s"full height $bestBlockHeight")
  }

  private def storageVersion(newModRow: EncryPersistentModifier) = ByteArrayWrapper(newModRow.id)

  protected def modifierValidation(m: EncryPersistentModifier, headerOpt: Option[EncryBlockHeader]): Try[Unit] = {
    val minimalHeight: Int = blockDownloadProcessor.minimalBlockHeight
    headerOpt.map(header => PayloadValidator.validate(m, header, minimalHeight).toTry)
      .getOrElse(Failure(RecoverableModifierError(s"Header for modifier $m is not defined")))
  }

  private def logStatus(toRemove: Seq[EncryBlock],
                        toApply: Seq[EncryBlock],
                        appliedBlock: EncryBlock,
                        prevBest: Option[EncryBlock]): Unit = {
    val toRemoveStr: String = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr: String = if (toApply.isEmpty) "" else {
      s" New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${prevBest.map(_.header.height).getOrElse(-1)}"
    }
    log.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }

  /** Validator for `BlockPayload` and `AdProofs` */
  object PayloadValidator extends ModifierValidator {

    def validate(m: EncryPersistentModifier, header: EncryBlockHeader, minimalHeight: Int): ValidationResult = {
      failFast
        .validate(!historyStorage.containsObject(m.id)) {
          fatal(s"Modifier ${m.encodedId} is already in history")
        }
        .validate(header.height >= minimalHeight) {
          fatal(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight")
        }
        .validate(header.isRelated(m)) {
          fatal(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}")
        }
        .validate(isSemanticallyValid(header.id) != Invalid) {
          fatal(s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid")
        }
        .result
    }
  }

}

object BlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[EncryPersistentModifier]]

  case class ToProcess(fullBlock: EncryBlock,
                       newModRow: EncryPersistentModifier,
                       newBestHeader: EncryBlockHeader,
                       newBestChain: Seq[EncryBlock],
                       blocksToKeep: Int)

}
