package encry.view.history.testingHistory

import com.google.common.primitives.Ints
import encry.consensus.ConsensusSchemeReaders
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.utils.NetworkTimeProvider
import encry.view.history.testingHistory.CleanHistory.HistoryProcessingInfo
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId, ModifierTypeId}
import HistoryErrors._
import cats.syntax.option._
import cats.syntax.either._
import scala.annotation.tailrec

trait CleanHistoryModifiersProcessor extends HistoryStorageApi {

  val blockDownloadingProcessor: CleanBlockDownloadingProcessor

  val isHeaderChainSynced: Boolean //todo upstream update

  val timeProvider: NetworkTimeProvider

  def processHeader(header: Header): Either[HistoryProcessingError, HistoryProcessingInfo] =
    getHeaderInfoToInsert(header) match {
      case Nil =>
        HistoryProcessingInfo(
          blockDownloadingProcessor,
          none[(ModifierTypeId, ModifierId)],
          none[PersistentModifier]
        ).asRight[HistoryProcessingError]
      case list: List[(StorageKey, StorageValue)] =>
        storage.bulkInsert(header.id, list, List(header)) //todo side effect
        bestHeaderId match {
          case Some(bestHeader) =>
            val modifierToApply: Option[PersistentModifier] =
              if (bestHeader sameElements header.id) none[PersistentModifier]
              else header.some

            if (header.height >= blockDownloadingProcessor.minimalBlockHeight)
              HistoryProcessingInfo(
                blockDownloadingProcessor,
                (Payload.modifierTypeId -> header.payloadId).some,
                modifierToApply
              ).asRight[HistoryProcessingError]
            else if (!isHeaderChainSynced && isNewHeader(header))
              HistoryProcessingInfo(
                blockDownloadingProcessor.updateBestBlockHeight(header.height),
                none[(ModifierTypeId, ModifierId)],
                modifierToApply
              ).asRight[HistoryProcessingError]
            else HistoryProcessingInfo(
              blockDownloadingProcessor,
              none[(ModifierTypeId, ModifierId)],
              modifierToApply
            ).asRight[HistoryProcessingError]
          case None =>
            HistoryProcessingError("History should always have best header after header application").asLeft[HistoryProcessingInfo]
        }
    }

  def processPayload(payload: Payload)

  def processBlock(block: Block)

  private def getHeaderInfoToInsert(header: Header): List[(StorageKey, StorageValue)] =
  //todo add cache
    if (header.isGenesis) {
      logger.info(s"Get info about genesis header ${header.encodedId}.")
      List(
        BestHeaderKey -> StorageValue @@ header.id,
        heightIdsKey(header.height) -> StorageValue @@ header.id,
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      )
    } else {
      logger.info(s"Header ${header.encodedId} isn't genesis. Processing it.")
      scoreOf(header.parentId).map { parentScore =>
        logger.info(s"Header's ${header.encodedId} parent's score contains in history. Calculating info to insert.")
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
            logger.info(s"Header ${header.encodedId} is from best chain. Calculating best headers ids.")
            bestHeaderIds(header, headerScore)
          } else {
            logger.info(s"Header ${header.encodedId} is from non best chain. Calculating non best headers ids.")
            nonBestHeaderIds(header, headerScore)
          }
        scoreToInsert :: heightToInsert :: newBestHeaderKeyToInsert ::: headerIdsToInsert
      }.getOrElse(List.empty[(StorageKey, StorageValue)])
    }

  private def nonBestHeaderIds(header: Header, score: Difficulty): List[(StorageKey, StorageValue)] = List(
    heightIdsKey(header.height) -> StorageValue @@ (header.id :: headerIdsAtHeightStorageApi(header.height)).flatten.toArray
  )

  private def bestHeaderIds(header: Header, score: Difficulty): List[(StorageKey, StorageValue)] = {
    //todo what is better, 0(n-e) search or 0(1) prepend
    val self: (StorageKey, StorageValue) = heightIdsKey(header.height) ->
      StorageValue @@ (header.id :: headerIdsAtHeightStorageApi(header.height).filterNot(_ sameElements header.id))
        .flatten.toArray
    val forkHeader: List[(StorageKey, StorageValue)] = headerByIdStorageApi(header.id)
      .toList.view //todo check .view correctness
      .flatMap(computeForkChain(header.height, _, header => isInBestChain(header)))
      //todo .filterNot(isInBestChain)
      .map(header => heightIdsKey(header.height) ->
        StorageValue @@ (header.id :: headerIdsAtHeightStorageApi(header.height).filterNot(_ sameElements header.id))
          .flatten.toArray)
      .toList
    self :: forkHeader
  }

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

  private def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier
}