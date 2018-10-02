package encry.view.history.processors

import encry.EncryApp.{settings, system}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.consensus.History.ProgressInfo
import encry.consensus.ModifierSemanticValidity.Invalid
import encry.local.explorer.BlockListener.NewBestBlock
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{Block, Header, HeaderChain}
import encry.utils.Logging
import encry.validation.{ModifierValidator, RecoverableModifierError, ValidationResult}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import io.circe.syntax._
import scala.util.{Failure, Try}

trait BlockProcessor extends BlockHeaderProcessor with Logging {

  import BlockProcessor._

  /** Id of header that contains transactions and proofs */
  override def bestBlockIdOpt: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  protected def getBlock(h: Header): Option[Block]

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected[history] def continuationHeaderChains(header: Header, filterCond: Header => Boolean): Seq[Seq[Header]]

  /** Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param modToApply - new part of the block we want to apply
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processBlock(fullBlock: Block,
                             modToApply: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] = {
    val bestFullChain: Seq[Block] = calculateBestFullChain(fullBlock)
    val newBestAfterThis: Header = bestFullChain.last.header
    logInfo(s"Going to process block: ${fullBlock.asJson}")
    logInfo(s"bestFullChain: ${bestFullChain.map(block => Algos.encode(block.id)).mkString(",")}")
    logInfo(s"newBestAfterThis: ${newBestAfterThis.asJson}")
    processing(ToProcess(fullBlock, modToApply, newBestAfterThis, bestFullChain, nodeSettings.blocksToKeep))
  }

  private def processing: BlockProcessing =
    processValidFirstBlock orElse
      processBetterChain orElse
      nonBestBlock

  private def processValidFirstBlock: BlockProcessing = {
    case ToProcess(fullBlock, newModRow, newBestHeader, newBestChain, _)
      if isValidFirstBlock(fullBlock.header) =>
      logInfo(s"Appending ${fullBlock.encodedId} as a valid first block")
      logStatus(Seq(), newBestChain, fullBlock, None)
      updateStorage(newModRow, newBestHeader.id)
      ProgressInfo(None, Seq.empty, newBestChain, Seq.empty)
  }

  private def processBetterChain: BlockProcessing = {
    case toProcess @ ToProcess(fullBlock, newModRow, newBestHeader, _, blocksToKeep)
      if bestBlockOpt.nonEmpty && isBetterChain(newBestHeader.id) =>
      logInfo(s"BestFullHeight: $bestBlockHeight")
      logInfo(s"BestBlock: ${bestBlockOpt.map(_.asJson)}")
      logInfo(s"bestBlockOpt.nonEmpty = true and isBetterChain(newBestHeader.id) = true for: ${fullBlock.asJson}")
      val prevBest: Block = bestBlockOpt.get
      logInfo(s"prevBest: ${prevBest.asJson}")
      val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(prevBest.header, newBestHeader)
//      logInfo(s"prevChain: ${prevChain.headers.map(header => Algos.encode(header.id) + "|" + header.height).mkString(",")}")
//      logInfo(s"newChain: ${newChain.headers.map(header => Algos.encode(header.id) + "|" + header.height).mkString(",")}")
      val toRemove: Seq[Block] = prevChain.tail.headers.flatMap(getBlock)
//      logInfo(s"newChain.length = ${newChain.length}")
//      logInfo(s"toRemove: ${toRemove.map(block => Algos.encode(block.id) + "|" + block.header.height).mkString(",")}")
      val toApply: Seq[Block] = newChain.tail.headers
        .flatMap(h => if (h == fullBlock.header) Some(fullBlock) else getBlock(h))
      logInfo(s"toApply: ${toApply.map(block => Algos.encode(block.id) + "|" + block.header.height).mkString(",")}")
      logInfo(s"toApply.lengthCompare(newChain.length - 1) != 0: ${toApply.lengthCompare(newChain.length - 1) != 0}")
      logInfo(s"toApply.length: ${toApply.length}")
      logInfo(s"toRemove.length: ${toRemove.length}")
      logInfo(s"toApply Heights: ${toApply.map(_.header.height).mkString(",")}")
      logInfo(s"newChain Heights: ${newChain.headers.map(_.height).mkString(",")}")
      if (toApply.lengthCompare(newChain.length - 1) != 0) nonBestBlock(toProcess)
      else {
        //application of this block leads to full chain with higher score
        logInfo(s"Appending ${fullBlock.encodedId} as a better chain")
        logStatus(toRemove, toApply, fullBlock, Some(prevBest))
        val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id)
        logInfo(s"!isInBestChain(fullBlock.id) = ${!isInBestChain(fullBlock.id)}")
        logInfo(s"scoreOf(fullBlock.id).flatMap(fbScore => bestHeaderIdOpt.flatMap(id => scoreOf(id).map(_ < fbScore)))." +
          s"getOrElse(false) = ${scoreOf(fullBlock.id)
            .flatMap(fbScore => bestHeaderIdOpt.flatMap(id => {
              logInfo(s"Score of bestHeader ${Algos.encode(id)} = ${scoreOf(id)}")
              logInfo(s"Score of bestBlock ${fullBlock.asJson} is ${fbScore}")
              scoreOf(id).map(_ < fbScore)
            }))
            .getOrElse(false)}")
        val updateBestHeader: Boolean =
            scoreOf(fullBlock.id)
              .flatMap(fbScore => bestHeaderIdOpt.flatMap(id => scoreOf(id).map(_ < fbScore)))
              .getOrElse(false)
        logInfo(s"newModRow: ${Algos.encode(newModRow.id)}")
        logInfo(s"newBestHeader.id: ${Algos.encode(newBestHeader.id)}")
        logInfo(s"updateBestHeader: $updateBestHeader")
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
    logInfo(s"Going to process: ${Algos.encode(id)}")
    val isBetter: Option[Boolean] = for {
      bestFullBlockId <- {
        logInfo(s"BestBlockOpt is: ${bestBlockOpt.map(_.asJson)}")
        bestBlockIdOpt
      }
      prevBestScore <- {
        logInfo(s"Score of ${Algos.encode(bestFullBlockId)} is: ${scoreOf(bestFullBlockId)}")
        scoreOf(bestFullBlockId)
      }
      score <- {
        logInfo(s"Score of ${Algos.encode(id)} is ${scoreOf(id)}")
        scoreOf(id)
      }
    } yield score > prevBestScore
    isBetter getOrElse false
  }

  private def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logInfo(s"Appending ${params.fullBlock.encodedId} as a non best block")
      logStatus(Seq(), Seq(), params.fullBlock, None)
      logInfo(s"Process block ${Algos.encode(params.fullBlock.id)} on height ${params.fullBlock.header.height} as non-best")
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
    logInfo(s"updateHeaderInfo: ${updateHeaderInfo}")
    val bestFullHeaderIdWrapped: ByteArrayWrapper = ByteArrayWrapper(bestFullHeaderId)
    val indicesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderIdWrapped, BestHeaderKey -> bestFullHeaderIdWrapped)
      else Seq(BestBlockKey -> bestFullHeaderIdWrapped)
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
    logInfo(s"Full block ${appliedBlock.encodedId} appended, " +
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
