package encry.view.history.processors

import cats.syntax.option._
import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.consensus.{ConsensusSchemeReaders, HistoryConsensus}
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.history.HistoryApi
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId, ModifierTypeId}

trait HeaderDefaultProcessorComponent extends HistoryHeaderProcessorComponent {
  this: HistoryApi =>

  override val headerProcessor: HeaderProcessor = new HeaderDefaultProcessor

  class HeaderDefaultProcessor extends HeaderProcessor {
    override def processHeader(h: Header): HistoryConsensus.ProgressInfo = getHeaderInfoUpdate(h) match {
      case dataToUpdate: Seq[_] if dataToUpdate.nonEmpty =>
        historyStorage.bulkInsert(h.id, dataToUpdate, Seq(h))
        getBestHeaderId match {
          case Some(bestHeaderId) =>
            ProgressInfo(none, Seq.empty, if (!bestHeaderId.sameElements(h.id)) Seq.empty else Seq(h), toDownload(h))
          case _ =>
            forceStopApplication(errorMessage = "Should always have best header after header application")
        }
      case _ => ProgressInfo(none, Seq.empty, Seq.empty, none)
    }

    private def getHeaderInfoUpdate(header: Header): Seq[(StorageKey, StorageValue)] = {
      addHeaderToCacheIfNecessary(header)
      if (header.isGenesis) {
        logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
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
          val bestHeaderHeight: Int         = getBestHeaderHeight
          val bestHeadersChainScore: BigInt = getBestHeadersChainScore
          val bestRow: Seq[(StorageKey, StorageValue)] =
            if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
              Seq(BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId))
            else Seq.empty
          val scoreRow: (StorageKey, StorageValue) = headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
          val heightRow: (StorageKey, StorageValue) =
            headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
          val headerIdsRow: Seq[(StorageKey, StorageValue)] =
            if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
              bestBlockHeaderIdsRow(header, score)
            else orphanedBlockHeaderIdsRow(header, score)
          Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow
        }.getOrElse(Seq.empty)
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

    private def toDownload(header: Header): Option[(ModifierTypeId, ModifierId)] =
    // Already synced and header is not too far back. Download required modifiers
      if (header.height >= blockDownloadProcessor.minimalBlockHeight) (Payload.modifierTypeId -> header.payloadId).some
      // Headers chain is synced after this header. Start downloading full blocks
      else if (!isHeadersChainSynced && isNewHeader(header)) {
        isHeadersChainSyncedVar = true
        blockDownloadProcessor.updateBestBlock(header)
        none
      } else none

    private def isNewHeader(header: Header): Boolean =
      timeProvider.estimatedTime - header.timestamp <
        settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier
  }

}
