package encry.view.history.processors

import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.view.history.processors.ValidationError.FatalValidationError._
import encry.view.history.processors.ValidationError.NonFatalValidationError._
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity
import scala.util.Try
import cats.syntax.either._

trait BlockProcessor extends HistoryExtension with StrictLogging {

  import BlockProcessor._

  protected val auxHistory: Boolean = false

  /** Id of header that contains transactions and proofs */
  //todo change description
  //override def bestBlockIdOpt: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  //def getBlockByHeader(h: Header): Option[Block]

  protected def isBlockDefined(h: Header): Boolean

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected[history] def continuationHeaderChains(header: Header, filterCond: Header => Boolean): Seq[Seq[Header]]

  //contains last n proccesed blocks
  var blocksCache: Map[ByteArrayWrapper, Block] = Map.empty[ByteArrayWrapper, Block]

  var blocksCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  /** Process full block when we have one.
    *
    * @param fullBlock  - block to process
    * @param modToApply - new part of the block we want to apply
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processBlock(fullBlock: Block,
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

  protected def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockId.isEmpty

  private def isBetterChain(id: ModifierId): Boolean = {
    val isBetter: Option[Boolean] = for {
      bestFullBlockId <- getBestBlockId
      //todo possible combine with getHeader(id: ModifierId)
      heightOfThisHeader <- lastAppliedHeadersCache.get(ByteArrayWrapper(id)).map(_.height).orElse(getHeightByHeaderId(id))
      prevBestScore <- scoreOf(bestFullBlockId)
      score <- scoreOf(id)
    } yield (getBestBlockHeight < heightOfThisHeader) || (getBestBlockHeight == heightOfThisHeader && score > prevBestScore)
    isBetter getOrElse false
  }

  private def addBlockToCacheIfNecessary(b: Block): Unit =
    if (b.header.height >= getBestBlockHeight - TestNetConstants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(b.id)} to header cache")
      val newBlocksIdsAtBlockHeight = blocksCacheIndexes.getOrElse(b.header.height, Seq.empty[ModifierId]) :+ b.id
      blocksCacheIndexes = blocksCacheIndexes + (b.header.height -> newBlocksIdsAtBlockHeight)
      blocksCache = blocksCache + (ByteArrayWrapper(b.id) -> b)
      // cleanup cache if necessary
      if (blocksCacheIndexes.size > TestNetConstants.MaxRollbackDepth) {
        blocksCacheIndexes.get(getBestBlockHeight - TestNetConstants.MaxRollbackDepth).foreach { blocksIds =>
          val wrappedIds = blocksIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup block cache from headers: ${blocksIds.map(Algos.encode).mkString(",")}")
          blocksCache = blocksCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        blocksCacheIndexes = blocksCacheIndexes - (getBestBlockHeight - TestNetConstants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${blocksCache.size}")
      logger.debug(s"headersCacheIndexes size: ${blocksCacheIndexes.size}")
    }

  private def nonBestBlock: BlockProcessing = {
    case params =>
      //Orphaned block or full chain is not initialized yet
      logStatus(Seq.empty, Seq.empty, params.fullBlock, None)
      historyStorage.bulkInsert(storageVersion(params.newModRow), Seq.empty, Seq(params.newModRow))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }


  private def calculateBestFullChain(block: Block): Seq[Block] = {
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isBlockDefined(h)).map(_.tail)
    logger.debug(s"continuations: ${continuations.map(seq => s"Seq contains: ${seq.length}").mkString(",")}")
    val chains: Seq[Seq[Block]] = continuations.map(_.filter(isBlockDefined).flatMap(getBlockByHeader))
    logger.debug(s"Chains: ${chains.map(chain => s"chain contain: ${chain.length}").mkString(",")}")
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  private def clipBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(h => headerIdsAtHeight(h))
      .flatMap(getHeaderById)
      .map(h => h.payloadId)
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(newModRow: PersistentModifier,
                            bestFullHeaderId: ModifierId,
                            updateHeaderInfo: Boolean = false): Unit = {
    val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
      else Seq(BestBlockKey -> bestFullHeaderId)
    historyStorage.bulkInsert(storageVersion(newModRow), indicesToInsert, Seq(newModRow))
  }

  private def storageVersion(newModRow: PersistentModifier): ModifierId = newModRow.id

  protected def modifierValidation(mod: PersistentModifier,
                                   headerOpt: Option[Header]): Either[ValidationError, PersistentModifier] = headerOpt
    .map(header => PayloadValidator.validate(mod, header, blockDownloadProcessor.minimalBlockHeight))
    .getOrElse(PayloadNonFatalValidationError(s"Header for ${mod.encodedId} doesn't contain in history").asLeft[PersistentModifier])

  private def logStatus(toRemove: Seq[Block],
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

  /** Validator for `BlockPayload` and `AdProofs` */
  object PayloadValidator {

    def validate(m: PersistentModifier,
                 header: Header,
                 minimalHeight: Int): Either[ValidationError, PersistentModifier] = for {
      _ <- Either.cond(!historyStorage.containsObject(m.id), (),
        PayloadFatalValidationError(s"Modifier ${m.encodedId} is already in history"))
      _ <- Either.cond(header.isRelated(m), (),
        PayloadFatalValidationError(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}"))
      _ <- Either.cond(isSemanticallyValid(header.id) != ModifierSemanticValidity.Invalid, (),
        PayloadFatalValidationError(s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid"))
      _ <- Either.cond(header.height >= minimalHeight, (),
        PayloadNonFatalValidationError(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight"))
    } yield m
  }

}

object BlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo[PersistentModifier]]

  case class ToProcess(fullBlock: Block,
                       newModRow: PersistentModifier,
                       newBestHeader: Header,
                       newBestChain: Seq[Block],
                       blocksToKeep: Int)
}