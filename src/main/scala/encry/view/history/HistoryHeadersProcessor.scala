package encry.view.history

import cats.syntax.option.none
import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.ConsensusSchemeReaders
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.history.History.AwaitingAppendToHistory
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.TaggedTypes.{ Difficulty, ModifierId }

trait HistoryHeadersProcessor extends HistoryApi {

  def processHeader(h: Header): (ProgressInfo, Option[AwaitingAppendToHistory]) = getHeaderInfoUpdate(h) match {
    case (bestHeaderId, dataToUpdate: Seq[_]) if dataToUpdate.nonEmpty =>
      //historyStorage.bulkInsert(h.id, dataToUpdate, Seq(h))
      bestHeaderId match {
        case Some(bestHeaderId) =>
          ProgressInfo(none, Seq.empty, if (!bestHeaderId.sameElements(h.id)) Seq.empty else Seq(h), toDownload(h)) ->
            Some(AwaitingAppendToHistory(dataToUpdate, h))
        case _ =>
          forceStopApplication(errorMessage = "Should always have best header after header application")
      }
    case _ => ProgressInfo(none, Seq.empty, Seq.empty, none) -> None
  }

  private def getHeaderInfoUpdate(header: Header): (Option[ModifierId], Seq[(StorageKey, StorageValue)]) = {
    addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Some(header.id) ->
        Seq(
          BestHeaderKey                                  -> StorageValue @@ header.id,
          heightIdsKey(settings.constants.GenesisHeight) -> StorageValue @@ header.id,
          headerHeightKey(header.id)                     -> StorageValue @@ Ints.toByteArray(settings.constants.GenesisHeight),
          headerScoreKey(header.id)                      -> StorageValue @@ header.difficulty.toByteArray
        )
    } else
      scoreOf(header.parentId).map { parentScore =>
        logger.info(s"getHeaderInfoUpdate for header $header")
        val score: Difficulty =
          Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
        val bestHeaderHeight: Int                = getBestHeaderHeight
        val bestHeadersChainScore: BigInt        = getBestHeadersChainScore
        val scoreRow: (StorageKey, StorageValue) = headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
        val heightRow: (StorageKey, StorageValue) =
          headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          Some(header.id) ->
            (Seq(scoreRow, heightRow) ++
              Seq(BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId)) ++
              bestBlockHeaderIdsRow(header, score))
        else getBestHeaderId -> (Seq(scoreRow, heightRow) ++ orphanedBlockHeaderIdsRow(header, score))
      }.getOrElse(None -> Seq.empty)
  }

  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New best header ${h.encodedId} with score: $score at height ${h.height}")
    val self: (StorageKey, StorageValue) =
      heightIdsKey(h.height) ->
        StorageValue @@ (Seq(h.id) ++ headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray
    val forkHeaders: Seq[(StorageKey, StorageValue)] = getHeaderById(h.parentId).toList.view
      .flatMap(headerChainBack(h.height, _, h => isInBestChain(h)).headers)
      .filterNot(isInBestChain)
      .map(
        header =>
          heightIdsKey(header.height) ->
            StorageValue @@ (Seq(header.id) ++
              headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)).flatten.toArray
      )
      .toList
    forkHeaders :+ self
  }

  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> StorageValue @@ (headerIdsAtHeight(h.height) :+ h.id).flatten.toArray)
  }
}
