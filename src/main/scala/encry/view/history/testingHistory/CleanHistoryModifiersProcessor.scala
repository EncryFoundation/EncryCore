package encry.view.history.testingHistory

import com.google.common.primitives.Ints
import encry.consensus.ConsensusSchemeReaders._
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.history.testingHistory.CleanHistory.HistoryProcessingInfo
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId, ModifierTypeId}
import HistoryErrors._
import cats.syntax.option._
import cats.syntax.either._
import scala.annotation.tailrec

trait CleanHistoryModifiersProcessor extends CleanHistoryApi {

  def processHeader(header: Header): Either[HistoryProcessingError, HistoryProcessingInfo] =
    getHeaderInfoToInsert(header) match {
      case Nil => HistoryProcessingInfo(blockDownloadingProcessor, isHeaderChainSynced).asRight[HistoryProcessingError]
      case elems: List[(StorageKey, StorageValue)] =>
        storage.bulkInsert(header.id, elems, List(header))
        bestHeaderId match {
          case Some(bestHeader) => computeResultAfterHeaderProcessing(header, bestHeader)
          case None => HistoryProcessingError("History has no best header after header application.").asLeft
        }
    }

  def processPayload(payload: Payload): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    blockByPayloadStorageApi(payload).map(processBlock)
      .getOrElse(putToHistory(payload))
  }

  private def putToHistory(payload: Payload): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    storage.insertObjects(Seq(payload))
    HistoryProcessingInfo(blockDownloadingProcessor, isHeaderChainSynced).asRight[HistoryProcessingError]
  }

  private def processBlock(block: Block): Either[HistoryProcessingError, HistoryProcessingInfo] =
    calculateBestFullChain(block) match {
      case Nil => HistoryProcessingInfo(blockDownloadingProcessor, isHeaderChainSynced).asRight[HistoryProcessingError]
      case fullChain@_ :+ last if isValidFirstBlock(block.header) => processFirstBlock(last.id, fullChain, block)
      case _ :+ last if isBestBlockDefined && isBetter(last.id) => processBlockFromBestChain(last.header, block)
      case _ :+ _ => processBlockFromNonBestChain(block)
    }

  private def processBlockFromBestChain(newBestHeader: Header,
                                        fullBlock: Block): Either[HistoryProcessingError, HistoryProcessingInfo] =
    headerOfBestBlockStorageApi.map { headerOfBestBlock =>
      commonBlockThenSuffixes(headerOfBestBlock, newBestHeader) match {
        case l@Left(ex) => ex.asLeft[HistoryProcessingInfo]
        case Right((oldChain: List[Header], newChain: List[Header])) =>
          val toApply: List[Block] = newChain.drop(1)
            .flatMap(h => if (h == fullBlock.header) fullBlock.some else blockByHeaderStorageApi(h))
          //todo add to cache toApply
          if (toApply.lengthCompare(newChain.length - 1) != 0) processBlockFromNonBestChain(fullBlock)
          else {
            val toRemove: List[Block] = oldChain.drop(1).flatMap(blockByHeaderStorageApi)
            val branchPoint: Option[ModifierId] =
              if (toRemove.nonEmpty) oldChain.headOption.map(_.id)
              else none[ModifierId]
            val heightOfBestHeader: Int = bestHeaderHeightStorageApi
            val updateBestHeaderOrNot: List[(StorageKey, StorageValue)] =
              if (isBetterThan(
                fullBlock.header.height,
                heightOfBestHeader,
                scoreOf(fullBlock.id).getOrElse(BigInt(0)),
                bestHeaderId.flatMap(id => scoreOf(id)).getOrElse(BigInt(0))))
                List(BestBlockKey -> StorageValue @@ newBestHeader.id, BestHeaderKey -> StorageValue @@ newBestHeader.id)
              else
                List(BestBlockKey -> StorageValue @@ newBestHeader.id)
            storage.bulkInsert(fullBlock.payload.id, updateBestHeaderOrNot, Seq(fullBlock.payload))
            if (settings.node.blocksToKeep >= 0) {
              val updatedBlockDownloadProcessor: CleanBlockDownloadingProcessor =
                blockDownloadingProcessor.updateBestBlockHeight(fullBlock.header.height)
              val lastSavedHeight: Int = updatedBlockDownloadProcessor.minimalBlockHeight
              val bestHeight: Int = toApply.lastOption.map(_.header.height).getOrElse(0)
              val diff: Int = bestHeight - lastSavedHeight
              storage.removeObjects(
                ((lastSavedHeight - diff) until lastSavedHeight).filter(_ >= 0)
                  .flatMap(headerIdsAtHeightStorageApi)
                  .flatMap(headerByIdStorageApi)
                  .map(_.payloadId)
              )
              HistoryProcessingInfo(
                updatedBlockDownloadProcessor, branchPoint, none, toApply, toRemove, isHeaderChainSynced
              ).asRight[HistoryProcessingError]
            } else HistoryProcessingInfo(
              blockDownloadingProcessor, branchPoint, none, toApply, toRemove, isHeaderChainSynced
            ).asRight[HistoryProcessingError]
          }
      }
    }.getOrElse(HistoryProcessingError(s"History doesn't contain header of best block.").asLeft[HistoryProcessingInfo])

  private def processFirstBlock(newBestHeaderId: ModifierId,
                                toApply: List[PersistentModifier],
                                block: Block): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    storage.bulkInsert(block.payload.id, Seq(BestBlockKey -> newBestHeaderId), Seq(block.payload))
    HistoryProcessingInfo(
      blockDownloadingProcessor,
      none[ModifierId],
      none[(ModifierTypeId, ModifierId)],
      toApply,
      List.empty[PersistentModifier],
      isHeaderChainSynced
    ).asRight[HistoryProcessingError]
  }

  private def processBlockFromNonBestChain(block: Block): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    storage.bulkInsert(block.payload.id, Seq.empty, Seq(block.payload))
    HistoryProcessingInfo(
      blockDownloadingProcessor,
      none[ModifierId],
      none[(ModifierTypeId, ModifierId)],
      List.empty[PersistentModifier],
      List.empty[PersistentModifier],
      isHeaderChainSynced
    ).asRight[HistoryProcessingError]
  }

  private def commonBlockThenSuffixes(firstHeader: Header,
                                      secondsHeader: Header): Either[HistoryProcessingError, (List[Header], List[Header])] = {
    val heightDelta: Int = Math.max(firstHeader.height - secondsHeader.height, 0)

    @tailrec def loop(otherChain: List[Header],
                      numberBack: Int): Either[HistoryProcessingError, (List[Header], List[Header])] =
      commonBlocksThenSuffixes(otherChain, firstHeader, numberBack + heightDelta) match {
        case (l1@ ::(head1, _), l2@ ::(head2, _)) if head1 == head2 => (l1 -> l2).asRight[HistoryProcessingError]
        case _ => //todo check this logic
          computeForkChain(numberBack, otherChain.head, _ => false) ::: otherChain.drop(1) match {
            case l@ ::(_, _) || Nil if !otherChain.head.isGenesis => loop(l, l.length)
            case _ => HistoryProcessingError("History has no best header after header application.").asLeft
          }
      }

    def commonBlocksThenSuffixes(otherChain: List[Header],
                                 startHeader: Header,
                                 limit: Int): (List[Header], List[Header]) = {
      val until: Header => Boolean = header => otherChain.exists(_.id sameElements header.id)
      val currentChain: List[Header] = computeForkChain(limit, startHeader, until)
      (currentChain, otherChain.dropWhile(_.id sameElements currentChain.head.id)) //todo check dropWhile correctness
    }

    loop(List(secondsHeader), 2) //todo why 2?
  }

  private def isBetter(id: ModifierId): Boolean = (for {
    bestFullBlockId <- bestBlockId
    heightOfThisHeader <- heightByHeaderStorageApi(id)
    prevBestScore <- scoreOf(bestFullBlockId) //todo add best score inserting to storage
    score <- scoreOf(id)
  } yield isBetterThan(bestBlockHeightStorageApi, heightOfThisHeader, score, prevBestScore)).getOrElse(false)

  private def calculateBestFullChain(block: Block): List[Block] =
    continuationHeaderChains(block.header, header => isBlockDefined(header))
      .view
      .map(chain => block :: chain.view.drop(1).flatMap(blockByHeaderStorageApi).toList)
      .maxBy(_.lastOption.flatMap(b => scoreOf(b.id)).getOrElse(BigInt(0))) //todo add either

  def continuationHeaderChains(header: Header, filterCond: Header => Boolean): List[List[Header]] = {
    @tailrec def loop(height: Int, acc: List[List[Header]]): List[List[Header]] =
      headerIdsAtHeightStorageApi(height + 1)
        .view
        .flatMap(headerByIdStorageApi).filter(filterCond).toList match {
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

  @tailrec final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeightStorageApi(height)
    .find(p)
    .flatMap(headerByIdStorageApi) match {
    case h@Some(_)                                         => h
    case None if height > settings.constants.GenesisHeight => loopHeightDown(height - 1, p)
    case n@None                                            => n
  }

  private def computeResultAfterHeaderProcessing(header: Header,
                                                 bestHeaderId: ModifierId): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    val modifierToApply: Option[PersistentModifier] =
      if (bestHeaderId sameElements header.id) none[PersistentModifier]
      else header.some
    if (header.height >= blockDownloadingProcessor.minimalBlockHeight)
      HistoryProcessingInfo(
        blockDownloadingProcessor,
        none[ModifierId],
        (Payload.modifierTypeId -> header.payloadId).some,
        modifierToApply.toList,
        List.empty[PersistentModifier],
        isHeaderChainSynced
      ).asRight[HistoryProcessingError]
    else if (!isHeaderChainSynced && isNewHeader(header))
      HistoryProcessingInfo(
        blockDownloadingProcessor.updateBestBlockHeight(header.height),
        none[ModifierId],
        none[(ModifierTypeId, ModifierId)],
        modifierToApply.toList,
        List.empty[PersistentModifier],
        isHeaderChainSynced = true
      ).asRight[HistoryProcessingError]
    else HistoryProcessingInfo(
      blockDownloadingProcessor,
      none[ModifierId],
      none[(ModifierTypeId, ModifierId)],
      modifierToApply.toList,
      List.empty[PersistentModifier],
      isHeaderChainSynced
    ).asRight[HistoryProcessingError]
  }

  private def getHeaderInfoToInsert(header: Header): List[(StorageKey, StorageValue)] =
  //todo add cache
    if (header.isGenesis)
      List(
        BestHeaderKey -> StorageValue @@ header.id,
        heightIdsKey(header.height) -> StorageValue @@ header.id,
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      )
    else scoreOf(header.parentId).map { parentScore =>
      val headerScore: Difficulty = Difficulty @@ (parentScore + consensusScheme.realDifficulty(header))
      val currentBestHeight: Int = bestHeaderHeightStorageApi
      val currentBestScore: BigInt = bestHeadersChainScore
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
    }.getOrElse(List.empty[(StorageKey, StorageValue)])

  private def nonBestHeaderIds(header: Header, score: Difficulty): List[(StorageKey, StorageValue)] = List(
    heightIdsKey(header.height) -> StorageValue @@ (header.id :: headerIdsAtHeightStorageApi(header.height)).flatten.toArray
  )

  private def bestHeaderIds(hc: Header, score: Difficulty): List[(StorageKey, StorageValue)] =
    (heightIdsKey(hc.height) -> (StorageValue @@ (hc.id :: headerIdsAtHeightStorageApi(hc.height)
      .filterNot(_ sameElements hc.id)).flatten.toArray)) ::
      headerByIdStorageApi(hc.id)
        .toList.view
        .flatMap(computeForkChain(hc.height, _, header => isInBestChain(header)))
        .map(h => heightIdsKey(h.height) ->
          StorageValue @@ (h.id :: headerIdsAtHeightStorageApi(h.height).filterNot(_ sameElements h.id)).flatten.toArray)
        .toList

  def computeForkChain(limit: Int, startHeader: Header, until: Header => Boolean): List[Header] = {
    @tailrec def loop(loopHeader: Header, acc: List[Header]): List[Header] =
      if (acc.length == limit || until(loopHeader)) acc
      else headerByIdStorageApi(loopHeader.parentId) match {
        case Some(parent) => loop(parent, parent :: acc)
        case None if acc.contains(loopHeader) => acc
        case _ => loopHeader :: acc
      }

    if (bestHeaderId.isEmpty || (limit == 0)) List.empty[Header]
    else loop(startHeader, List(startHeader))
  }

  private def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadingProcessor.minimalBlockHeight && bestBlockId.isEmpty

  private def isNewHeader(h: Header): Boolean =
    timeProvider.estimatedTime - h.timestamp <
      settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier

  private def isBetterThan(currentHeight: Int, bestHeight: Int, currentScore: BigInt, bestScore: BigInt): Boolean =
    (currentHeight > bestHeight) || (currentHeight == bestHeight && currentScore > bestScore)
}