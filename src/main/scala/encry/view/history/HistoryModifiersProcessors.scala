package encry.view.history

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import cats.syntax.option._
import scala.annotation.tailrec
import cats.syntax.either._
import encry.consensus.ConsensusSchemeReaders.consensusScheme
import encry.view.history.History._
import encry.view.history.ValidationError._
import org.encryfoundation.common.utils.Algos

trait HistoryModifiersProcessors extends HistoryCacheApi {

  def processHeader(header: Header): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    logger.info(s"Have been starting applying header to history at height ${header.height}.")
    getHeaderInfoToInsert(header) match {
      case Nil =>
        logger.info(s"Got empty header info for header ${header.encodedId}. Returned same history case class.")
        HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced).asRight[HistoryProcessingError]
      case elems: List[(StorageKey, StorageValue)] =>
        storage.bulkInsert(header.id, elems, List(header))
        bestHeaderIdStorageApi match {
          case Some(bestHeader) =>
            logger.info(s"Inserted info about header ${header.encodedId}. Computed result.")
            computeResultAfterHeaderProcessing(header, bestHeader)
          case None =>
            logger.info(s"History didn't contain best header after header insertion.")
            HistoryProcessingError("History didn't contain best header after header insertion.").asLeft
        }
    }
  }

  def processPayload(p: Payload): Either[HistoryProcessingError, HistoryProcessingInfo] =
    blockByPayloadOpt(p).map { b =>
      logger.info(s"Found block for payload ${p.encodedId}. Have been starting applying block ${b.encodedId} with height ${b.header.height} to history.")
      processBlock(b)
    }.getOrElse {
      logger.info(s"Didn't find block for payload ${p.encodedId}. Inserted info about this payload to history.")
      putToHistory(p)
    }

  private def processBlock(block: Block): Either[HistoryProcessingError, HistoryProcessingInfo] =
    calculateBestFullChain(block) match {
      case Nil =>
        logger.info(s"Best full chain for block ${block.encodedId} was empty. Returned same history case class.")
        HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced).asRight[HistoryProcessingError]
      case fullChain@_ :+ last if isValidFirstBlock(block.header) =>
        logger.info(s"Got first valid block ${block.encodedId}. Have been starting its applying.")
        processFirstBlock(last.id, fullChain, block)
      case _ :+ last if isBestBlockDefined && isBetter(last.id) =>
        logger.info(s"Got block ${block.encodedId} from best chain. Have been starting its applying.")
        processBlockFromBestChain(last.header, block)
      case _ =>
        logger.info(s"Got block ${block.encodedId} from non best chain. Have been starting its applying.")
        processBlockFromNonBestChain(block)
    }

  private def processFirstBlock(newBestHeaderId: ModifierId,
                                toApply: List[PersistentModifier],
                                block: Block): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    storage.bulkInsert(block.payload.id, Seq(BestBlockKey -> newBestHeaderId), Seq(block.payload))
    logger.info(s"First block ${block.encodedId} applied successfully.")
    HistoryProcessingInfo(blockDownloadProcessor, toApply, isHeaderChainSynced).asRight[HistoryProcessingError]
  }

  private def processBlockFromBestChain(newBestHeader: Header,
                                        fullBlock: Block): Either[HistoryProcessingError, HistoryProcessingInfo] =
    headerOfBestBlockOpt.map { headerOfBestBlock =>
      logger.info(s"Header ${headerOfBestBlock.encodedId} of best block was found while best block ${fullBlock.encodedId} was processing.")
      commonBlockThenSuffixes(headerOfBestBlock, newBestHeader) match {
        case Left(ex) =>
          logger.info(s"Exception ${ex.error} was thrown during computation of common blocks for block ${fullBlock.encodedId}.")
          ex.asLeft[HistoryProcessingInfo]
        case Right((oldChain: List[Header], newChain: List[Header])) =>
          logger.info(s"Found common blocks for block ${fullBlock.encodedId}.")
          val toApply: List[Block] = newChain.drop(1)
            .flatMap(h => if (h == fullBlock.header) fullBlock.some else blockByHeaderOpt(h))
          toApply.foreach(addBlockToCacheIfNecessary)
          if (toApply.lengthCompare(newChain.length - 1) != 0) {
            logger.info(s"During block ${fullBlock.encodedId} processing it was from non best chain.")
            processBlockFromNonBestChain(fullBlock)
          }
          else {
            val toRemove: List[Block] = oldChain.drop(1).flatMap(blockByHeaderOpt)
            val branchPoint: Option[ModifierId] =
              if (toRemove.nonEmpty) oldChain.headOption.map(_.id)
              else none[ModifierId]
            val heightOfBestHeader: Int = getBestHeaderHeight
            val updateBestHeaderOrNot: List[(StorageKey, StorageValue)] =
              if (isBetterThan(
                fullBlock.header.height,
                heightOfBestHeader,
                scoreOf(fullBlock.id).getOrElse(BigInt(0)),
                bestHeaderIdStorageApi.flatMap(scoreOf).getOrElse(BigInt(0))))
                List(BestBlockKey -> StorageValue @@ newBestHeader.id, BestHeaderKey -> StorageValue @@ newBestHeader.id)
              else
                List(BestBlockKey -> StorageValue @@ newBestHeader.id)
            storage.bulkInsert(fullBlock.payload.id, updateBestHeaderOrNot, Seq(fullBlock.payload))
            if (settings.node.blocksToKeep >= 0) {
              val updatedBlockDownloadProcessor: BlockDownloadProcessor =
                blockDownloadProcessor.updateBestBlockHeight(fullBlock.header.height)
              val lastSavedHeight: Int = updatedBlockDownloadProcessor.minimalBlockHeight
              val bestHeight: Int = toApply.lastOption.map(_.header.height).getOrElse(0)
              val diff: Int = bestHeight - lastSavedHeight
              storage.removeObjects(
                (((lastSavedHeight - diff) until lastSavedHeight).filter(_ >= 0))
                  .flatMap(headerIdsAtHeight)
                  .flatMap(headerByIdOpt)
                  .map(_.payloadId)
              )
              logger.info(s"Finished processing block ${fullBlock.encodedId} from best chain. BlocksToKeep >= 0.")
              HistoryProcessingInfo(
                updatedBlockDownloadProcessor, branchPoint, none, toApply, toRemove, isHeaderChainSynced
              ).asRight[HistoryProcessingError]
            } else {
              logger.info(s"Finished processing block ${fullBlock.encodedId} from non best chain. BlocksToKeep >= 0.")
              HistoryProcessingInfo(
                blockDownloadProcessor, branchPoint, none, toApply, toRemove, isHeaderChainSynced
              ).asRight[HistoryProcessingError]
            }
          }
      }
    }.getOrElse {
      logger.info(s"Finished block ${fullBlock.encodedId} processing with exception: History didn't contain header of best block.")
      HistoryProcessingError(s"History didn't contain header of best block.").asLeft[HistoryProcessingInfo]
    }

  private def processBlockFromNonBestChain(block: Block): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    storage.bulkInsert(block.payload.id, Seq.empty, Seq(block.payload))
    logger.info(s"Finished non best block ${block.encodedId} processing.")
    HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced).asRight[HistoryProcessingError]
  }

  private def calculateBestFullChain(block: Block): List[Block] =
    continuationHeaderChains(block.header, header => isBlockDefined(header))
      .view
      .map(chain => block :: chain.view.drop(1).flatMap(blockByHeaderOpt).toList)
      .maxBy(_.lastOption.flatMap(b => scoreOf(b.id)).getOrElse(BigInt(0)))

  def continuationHeaderChains(header: Header, filterCond: Header => Boolean): List[List[Header]] = {
    @tailrec def loop(height: Int, acc: List[List[Header]]): List[List[Header]] =
      headerIdsAtHeight(height + 1)
        .view
        .flatMap(headerByIdOpt).filter(filterCond).toList match {
        case Nil => acc
        case nextHeightHeaders@ ::(_, _) =>
          val updatedChains: List[List[Header]] = nextHeightHeaders.flatMap(header =>
            acc.find(_.headOption.exists(_.id sameElements header.parentId)).map(header :: _))
          val notUpdated: List[List[Header]] = acc.filter(chain =>
            !nextHeightHeaders.exists(_.parentId sameElements chain.head.id))
          loop(height + 1, updatedChains ::: notUpdated)
      }

    loop(header.height, List(List(header)))
  }

  private def getHeaderInfoToInsert(header: Header): List[(StorageKey, StorageValue)] = {
    addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Processed genesis header ${header.encodedId}.")
      List(
        BestHeaderKey               -> StorageValue @@ header.id,
        heightIdsKey(header.height) -> StorageValue @@ header.id,
        headerHeightKey(header.id)  -> StorageValue @@ Ints.toByteArray(header.height),
        headerScoreKey(header.id)   -> StorageValue @@ header.difficulty.toByteArray
      )
    } else scoreOf(header.parentId).map { parentScore =>
      logger.info(s"Have been starting processing non genesis header ${header.encodedId}.")
      val headerScore: Difficulty = Difficulty @@ (parentScore + consensusScheme.realDifficulty(header))
      val currentBestHeight: Int = getBestHeaderHeight
      val currentBestScore: BigInt = getBestHeadersChainScore
      val newBestHeaderKeyToInsert: List[(StorageKey, StorageValue)] =
        if (isBetterThan(header.height, currentBestHeight, headerScore, currentBestScore))
          List(BestHeaderKey -> StorageValue @@ header.id)
        else
          List.empty[(StorageKey, StorageValue)]
      val newScoreToInsert: (StorageKey, StorageValue) =
        headerScoreKey(header.id) -> StorageValue @@ headerScore.toByteArray
      val heightToInsert: (StorageKey, StorageValue) =
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
      val headerIdsToInsert: List[(StorageKey, StorageValue)] =
        if (newBestHeaderKeyToInsert.nonEmpty) bestHeaderIds(header, headerScore)
        else nonBestHeaderIds(header, headerScore)
      newScoreToInsert :: heightToInsert :: newBestHeaderKeyToInsert ::: headerIdsToInsert
    }.getOrElse {
      logger.info(s"Header ${header.encodedId} with parent ${Algos.encode(header.parentId)} didn't contain in history.")
      List.empty[(StorageKey, StorageValue)]
    }
  }

  private def computeResultAfterHeaderProcessing(header: Header,
                                                 bestHeaderId: ModifierId): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    val modifierToApply: Option[PersistentModifier] =
      if (bestHeaderId sameElements header.id) header.some
      else none[PersistentModifier]
    if (header.height >= blockDownloadProcessor.minimalBlockHeight)
      HistoryProcessingInfo(
        blockDownloadProcessor,
        none[ModifierId],
        (Payload.modifierTypeId -> header.payloadId).some,
        modifierToApply.toList,
        List.empty[PersistentModifier],
        isHeaderChainSynced
      ).asRight[HistoryProcessingError]
    else if (!isHeaderChainSynced && isNewHeader(header))
      HistoryProcessingInfo(
        blockDownloadProcessor.updateBestBlockHeight(header.height),
        modifierToApply.toList,
        isHeaderChainSynced = true
      ).asRight[HistoryProcessingError]
    else HistoryProcessingInfo(blockDownloadProcessor, modifierToApply.toList, isHeaderChainSynced).asRight[HistoryProcessingError]
  }

  private def putToHistory(payload: Payload): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    storage.insertObjects(Seq(payload))
    HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced).asRight
  }

  private def isBetter(id: ModifierId): Boolean = (for {
    bestFullBlockId    <- bestBlockIdStorageApi
    heightOfThisHeader <- heightByHeaderStorageApi(id)
    prevBestScore      <- scoreOf(bestFullBlockId) //todo add best score inserting to storage
    score              <- scoreOf(id)
  } yield isBetterThan(bestBlockHeightStorageApi, heightOfThisHeader, score, prevBestScore)).getOrElse(false)

  private def nonBestHeaderIds(header: Header, score: Difficulty): List[(StorageKey, StorageValue)] = {
    logger.info(s"Header ${header.encodedId} was from non best chain.")
    List(heightIdsKey(header.height) -> StorageValue @@ (header.id :: headerIdsAtHeight(header.height)).flatten.toArray)
  }

  private def bestHeaderIds(hc: Header, score: Difficulty): List[(StorageKey, StorageValue)] = {
    logger.info(s"Header ${hc.encodedId} was from best chain.")
    (heightIdsKey(hc.height) -> (StorageValue @@ (hc.id :: headerIdsAtHeight(hc.height)
      .filterNot(_ sameElements hc.id)).flatten.toArray)) ::
      headerByIdOpt(hc.id)
        .toList.view
        .flatMap(computeForkChain(hc.height, _, header => isInBestChain(header)))
        .map(h => heightIdsKey(h.height) ->
          StorageValue @@ (h.id ::  headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray)
        .toList
  }

  private def isBetterThan(currentHeight: Int, bestHeight: Int, currentScore: BigInt, bestScore: BigInt): Boolean =
    (currentHeight > bestHeight) || (currentHeight == bestHeight && currentScore > bestScore)

  private def isNewHeader(h: Header): Boolean =
    timeProvider.estimatedTime - h.timestamp <
      settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier

  private def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && bestBlockIdStorageApi.isEmpty
}