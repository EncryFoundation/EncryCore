package encry.view.history.testingHistory

import com.google.common.primitives.Ints
import encry.consensus.ConsensusSchemeReaders
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
      case Nil => HistoryProcessingInfo(
        blockDownloadingProcessor,
        none[(ModifierTypeId, ModifierId)],
        none[PersistentModifier],
        isHeaderChainSynced
      ).asRight[HistoryProcessingError]
      case elems: List[(StorageKey, StorageValue)] =>
        storage.bulkInsert(header.id, elems, List(header))
        bestHeaderId match {
          case Some(bestHeader) => computeResultAfterHeaderProcessing(header, bestHeader)
          case None => HistoryProcessingError("History has no best header after header application").asLeft
        }
    }

  def processPayload(payload: Payload): Unit = {
    blockByPayloadStorageApi(payload).map(processBlock)
  }

  private def processBlock(block: Block): Unit = {
    val bestFullChain: List[Block] = calculateBestFullChain(block) //todo do we need such chain calculating
    //todo add block to cache here
    bestFullChain.lastOption.map(_.header) match {
      case Some(header) if isValidFirstBlock(block.header) => //todo we don't need to check this condition all time
        //todo in this case is it possible to have non empty blocks chain
        processFirstBlock(block, header.id)
      case Some(header) if isBestBlockDefined && isBetter(header.id) =>
      case Some(_) => processBlockFromNonBestChain(block)
      case None => //todo add return type
    }
  }

  private def processBlockFromBestChain() = {}

  private def commonBlockThenSuffixes(firstHeader: Header, secondsHeader: Header) = {

    def commonBlocksThenSuffixes(otherChain: List[Header], startHeader: Header, limit: Int) = {

    }
  }

  private def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): List[Header] = {
    @tailrec def loop(header: Header, acc: List[Header]): List[Header] =
      if (acc.length == limit || until(header)) acc
      else headerByIdStorageApi(header.parentId) match {
        case Some(parentHeader) => loop(parentHeader, parentHeader :: acc)
        case None if acc.contains(header) => acc
        case _ => header :: acc
      }
    if (bestHeaderId.isEmpty || limit == 0) List.empty[Header]
    else loop(startHeader, List(startHeader))
  }

  private def processFirstBlock(block: Block, newBestHeaderId: ModifierId): Unit = {
    storage.bulkInsert(block.payload.id, Seq(BestBlockKey -> newBestHeaderId), Seq(block.payload))
  }

  //todo add return type
  private def processBlockFromNonBestChain(block: Block): Unit = {
    storage.bulkInsert(block.payload.id, Seq.empty, Seq(block.payload))
  }

  private def isBetter(id: ModifierId): Boolean = (for {
    bestFullBlockId <- bestBlockId
    heightOfThisHeader <- heightByHeaderStorageApi(id)
    prevBestScore <- scoreOf(bestFullBlockId) //todo add best score inserting to the storage
    score <- scoreOf(id)
    bestBlockHeight = bestBlockHeightStorageApi
  } yield (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore))
    .getOrElse(false)

  private def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadingProcessor.minimalBlockHeight && bestBlockId.isEmpty

  private def calculateBestFullChain(block: Block): List[Block] =
    continuationHeaderChains(block.header, header => isBlockDefined(header))
      .view
      .map(chain => block :: chain.view.drop(1).flatMap(blockByHeaderStorageApi).toList)
      .maxBy(_.lastOption.flatMap(b => scoreOf(b.id)).getOrElse(BigInt(0)))

  private def continuationHeaderChains(header: Header, filterCond: Header => Boolean): List[List[Header]] = {
    @tailrec def loop(height: Int, acc: List[List[Header]]): List[List[Header]] = {
      val nextHeightHeaders: List[Header] = headerIdsAtHeightStorageApi(height + 1)
        .view
        .flatMap(headerByIdStorageApi)
        .filter(filterCond)
        .toList
      if (nextHeightHeaders.nonEmpty) {
        //todo make attention here. Try to improve updatedChains to notUpdated
        val updatedChains: List[List[Header]] = nextHeightHeaders.flatMap(header =>
          acc.find(_.headOption.exists(_.id sameElements header.parentId)).map(header :: _))
        val notUpdated: List[List[Header]] = acc.filter(chain =>
          !nextHeightHeaders.exists(_.parentId sameElements chain.head.id))
        loop(height + 1, updatedChains ::: notUpdated)
      } else acc
    }

    loop(header.height, List(List(header)))
  }

  private def computeResultAfterHeaderProcessing(header: Header,
                                                 bestHeaderId: ModifierId): Either[HistoryProcessingError, HistoryProcessingInfo] = {
    val modifierToApply: Option[PersistentModifier] =
      if (bestHeaderId sameElements header.id) none[PersistentModifier]
      else header.some
    if (header.height >= blockDownloadingProcessor.minimalBlockHeight)
      HistoryProcessingInfo(
        blockDownloadingProcessor,
        (Payload.modifierTypeId -> header.payloadId).some,
        modifierToApply,
        isHeaderChainSynced
      ).asRight[HistoryProcessingError]
    else if (!isHeaderChainSynced && isNewHeader(header))
      HistoryProcessingInfo(
        blockDownloadingProcessor.updateBestBlockHeight(header.height),
        none[(ModifierTypeId, ModifierId)],
        modifierToApply,
        isHeaderChainSynced = true
      ).asRight[HistoryProcessingError]
    else HistoryProcessingInfo(
      blockDownloadingProcessor,
      none[(ModifierTypeId, ModifierId)],
      modifierToApply,
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
    else scoreOf(header.parentId).map {
      parentScore =>
        val headerScore: Difficulty =
          Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
        val currentBestHeight: Int = bestHeaderHeightStorageApi
        val currentBestScore: BigInt = bestHeadersChainScore
        val newBestHeaderKeyToInsert: List[(StorageKey, StorageValue)] =
          if ((header.height > currentBestHeight) || (header.height == currentBestHeight && headerScore > currentBestScore))
            List(BestHeaderKey -> StorageValue @@ header.id)
          else
            List.empty[(StorageKey, StorageValue)]
        val scoreToInsert: (StorageKey, StorageValue) =
          headerScoreKey(header.id) -> StorageValue @@ headerScore.toByteArray
        val heightToInsert: (StorageKey, StorageValue) =
          headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
        val headerIdsToInsert: List[(StorageKey, StorageValue)] =
          if (newBestHeaderKeyToInsert.nonEmpty) bestHeaderIds(header, headerScore)
          else nonBestHeaderIds(header, headerScore)
        scoreToInsert :: heightToInsert :: newBestHeaderKeyToInsert ::: headerIdsToInsert
    }.getOrElse(List.empty[(StorageKey, StorageValue)])

  private def nonBestHeaderIds(header: Header, score: Difficulty): List[(StorageKey, StorageValue)] = List(
    heightIdsKey(header.height) -> StorageValue @@ (header.id :: headerIdsAtHeightStorageApi(header.height)).flatten.toArray
  )

  private def bestHeaderIds(hc: Header, score: Difficulty): List[(StorageKey, StorageValue)] =
    heightIdsKey(hc.height) -> (StorageValue @@ (hc.id :: headerIdsAtHeightStorageApi(hc.height).filterNot(_ sameElements hc.id)).flatten.toArray) ::
      headerByIdStorageApi(hc.id)
        .toList.view
        .flatMap(computeForkChain(hc.height, _, header => isInBestChain(header)))
        .map(h => heightIdsKey(h.height) ->
          StorageValue @@ (h.id :: headerIdsAtHeightStorageApi(h.height).filterNot(_ sameElements h.id)).flatten.toArray)
        .toList

  private def computeForkChain(limit: Int, startHeader: Header, until: Header => Boolean): List[Header] = {
    @tailrec def loop(localHeader: Header, acc: List[Header]): List[Header] =
      if (acc.length == limit || until(localHeader)) acc
      else headerByIdStorageApi(localHeader.parentId) match {
        case Some(parent) => loop(parent, parent :: acc)
        case None if acc.contains(localHeader) => acc
        case _ => localHeader :: acc
      }

    if (bestHeaderId.isEmpty || (limit == 0)) List.empty[Header]
    else loop(startHeader, List(startHeader)) //todo .reverse check correctness
  }

  private def isNewHeader(h: Header): Boolean =
    timeProvider.estimatedTime - h.timestamp <
      settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier
}