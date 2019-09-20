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
    blockByPayloadStorageApi(payload).flatMap(block =>

    )
  }

  private def processBlock(block: Block): Unit = {

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
    if (header.isGenesis) {
      logger.info(s"Get info about genesis header ${
        header.encodedId
      }.")
      List(
        BestHeaderKey -> StorageValue @@ header.id,
        heightIdsKey(header.height) -> StorageValue @@ header.id,
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      )
    } else {
      logger.info(s"Header ${
        header.encodedId
      } isn't genesis. Processing it.")
      scoreOf(header.parentId).map {
        parentScore =>
          logger.info(s"Header's ${
            header.encodedId
          } parent's score contains in history. Calculating info to insert.")
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
            if (newBestHeaderKeyToInsert.nonEmpty) {
              logger.info(s"Header ${
                header.encodedId
              } is from best chain. Calculating best headers ids.")
              bestHeaderIds(header, headerScore)
            } else {
              logger.info(s"Header ${
                header.encodedId
              } is from non best chain. Calculating non best headers ids.")
              nonBestHeaderIds(header, headerScore)
            }
          scoreToInsert :: heightToInsert :: newBestHeaderKeyToInsert ::: headerIdsToInsert
      }.getOrElse(List.empty[(StorageKey, StorageValue)])
    }

  private def nonBestHeaderIds(header: Header, score: Difficulty): List[(StorageKey, StorageValue)] = List(
    heightIdsKey(header.height) -> StorageValue @@ (header.id :: headerIdsAtHeightStorageApi(header.height)).flatten.toArray
  )

  private def bestHeaderIds(hc: Header, score: Difficulty): List[(StorageKey, StorageValue)] =
    StorageValue @@ (hc.id :: headerIdsAtHeightStorageApi(hc.height).filterNot(_ sameElements hc.id)).flatten.toArray ::
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