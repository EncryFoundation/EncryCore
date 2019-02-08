package encry.view.history.processors

import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.system
import encry.utils.CoreTaggedTypes.ModifierId
import encry.consensus.History.ProgressInfo
import encry.consensus.ModifierSemanticValidity.Invalid
import encry.local.explorer.BlockListener.NewBestBlock
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{Block, Header, HeaderChain}
import encry.validation.{ModifierValidator, RecoverableModifierError, ValidationResult}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos

import scala.util.{Failure, Try}

trait BlockProcessor extends BlockHeaderProcessor with StrictLogging {

  import BlockProcessor._

  /** Id of header that contains transactions and proofs */
  override def bestBlockIdOpt: Option[ModifierId] = {
    val a = historyStorage.get(BestBlockKey).map(ModifierId @@ _)
    //println(s"${Algos.encode(a.get)}")
    a
  }

  protected def getBlock(h: Header): Option[Block]

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected[history] def continuationHeaderChains(header: Header, filterCond: Header => Boolean): Seq[Seq[Header]]

  /** Process full block when we have one.
    *
    * @param fullBlock  - block to process
    * @param modToApply - new part of the block we want to apply
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processBlock(fullBlock: Block,
                             modToApply: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] = {
    val bestFullChain: Seq[Block] = calculateBestFullChain(fullBlock)
    val newBestAfterThis: Header = bestFullChain.last.header
    processing(ToProcess(fullBlock, modToApply, newBestAfterThis, bestFullChain, settings.node.blocksToKeep))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestHeader, newBestChain, _)
      if isValidFirstBlock(fullBlock.header) =>
      logger.info(s"Appending ${fullBlock.encodedId} as a valid first block")
      logStatus(Seq(), newBestChain, fullBlock, None)
      updateStorage(newModRow, newBestHeader.id)
      ProgressInfo(None, Seq.empty, newBestChain, Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case toProcess@ToProcess(fullBlock, newModRow, newBestHeader, _, blocksToKeep)
      if bestBlockOpt.nonEmpty && isBetterChain(newBestHeader.id) =>
      val prevBest: Block = bestBlockOpt.get
      val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(prevBest.header, newBestHeader)
      val toRemove: Seq[Block] = prevChain.tail.headers.flatMap(getBlock)
      val toApply: Seq[Block] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getBlock(h))
      if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(toProcess)
      else {
        //application of this block leads to full chain with higher score
        logger.info(s"Appending ${fullBlock.encodedId}|${fullBlock.header.height} as a better chain")
        logStatus(toRemove, toApply, fullBlock, Some(prevBest))
        val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
        val updateBestHeader: Boolean =
          (fullBlock.header.height > bestHeaderHeight) || (
            (fullBlock.header.height == bestHeaderHeight) &&
              scoreOf(fullBlock.id)
                .flatMap(fbScore => bestHeaderIdOpt.flatMap(id => scoreOf(id).map(_ < fbScore)))
                .getOrElse(false))
        updateStorage(newModRow, newBestHeader.id, updateBestHeader)

        if (settings.postgres.exists(_.enableSave))
          system.actorSelection("/user/blockListener") ! NewBestBlock(fullBlock.header.height)

        if (blocksToKeep >= 0) {
          val lastKept: Int = blockDownloadProcessor.updateBestBlock(fullBlock.header)
          val bestHeight: Int = toApply.last.header.height
          val diff: Int = bestHeight - prevBest.header.height
          clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
        }
        ProgressInfo(branchPoint, toRemove, toApply, Seq.empty)
      }
  }

  protected def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && bestBlockIdOpt.isEmpty

  private def isBetterChain(id: ModifierId): Boolean = {
    val isBetter: Option[Boolean] = for {
      bestFullBlockId <- bestBlockIdOpt
      heightOfThisHeader <- typedModifierById[Header](id).map(_.height)
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(id)
    } yield (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore)
    isBetter getOrElse false
  }

  private def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logger.info(s"Appending ${params.fullBlock.encodedId}. Height: ${params.fullBlock.header.height} as a non best block.")
      logStatus(Seq(), Seq(), params.fullBlock, None)
      historyStorage.bulkInsert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def calculateBestFullChain(block: Block): Seq[Block] = {
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => getBlock(h).nonEmpty).map(_.tail)
    val chains: Seq[Seq[Block]] = continuations.map(_.map(getBlock).takeWhile(_.nonEmpty).flatten)
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  private def clipBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[Header](id))
      .flatMap(h => Seq(h.adProofsId, h.payloadId))
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(newModRow: EncryPersistentModifier,
                            bestFullHeaderId: ModifierId,
                            updateHeaderInfo: Boolean = false): Unit = {
    val bestFullHeaderIdWrapped: ByteArrayWrapper = ByteArrayWrapper(bestFullHeaderId)
    val indicesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (updateHeaderInfo) {
        println(s"updateHeaderInfo -> updateStorage -> ${BestBlockKey}")
        Seq(BestBlockKey -> bestFullHeaderIdWrapped, BestHeaderKey -> bestFullHeaderIdWrapped)
      }
      else {
        println(s"updateHeaderInfo -> updateStorage -> -> ELSE _> ${BestBlockKey}")
        Seq(BestBlockKey -> bestFullHeaderIdWrapped)
      }
    historyStorage.bulkInsert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
  }

  private def storageVersion(newModRow: EncryPersistentModifier) = ByteArrayWrapper(newModRow.id)

  protected def modifierValidation(m: EncryPersistentModifier, headerOpt: Option[Header]): Try[Unit] = {
    val minimalHeight: Int = blockDownloadProcessor.minimalBlockHeight
    headerOpt.map(header => PayloadValidator.validate(m, header, minimalHeight).toTry)
      .getOrElse(Failure(RecoverableModifierError(s"Header for modifier $m is not defined")))
  }

  private def logStatus(toRemove: Seq[Block],
                        toApply: Seq[Block],
                        appliedBlock: Block,
                        prevBest: Option[Block]): Unit = {
    val toRemoveStr: String = if (toRemove.isEmpty) "" else s" and to remove ${toRemove.length}"
    val newStatusStr: String = if (toApply.isEmpty) "" else {
      s" New best block is ${toApply.last.header.encodedId} " +
        s"with height ${toApply.last.header.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${prevBest.map(_.header.height).getOrElse(-1)}"
    }
    logger.info(s"Full block ${appliedBlock.encodedId} appended, " +
      s"going to apply ${toApply.length}$toRemoveStr modifiers.$newStatusStr")
  }

  /** Validator for `BlockPayload` and `AdProofs` */
  object PayloadValidator extends ModifierValidator {

    def validate(m: EncryPersistentModifier, header: Header, minimalHeight: Int): ValidationResult = {
      failFast
        .validate(!historyStorage.containsObject(m.id)) {
          fatal(s"Modifier ${m.encodedId} is already in history")
        }
        .validate(header.height >= minimalHeight) {
          error(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight")
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

  case class ToProcess(fullBlock: Block,
                       newModRow: EncryPersistentModifier,
                       newBestHeader: Header,
                       newBestChain: Seq[Block],
                       blocksToKeep: Int)

}
