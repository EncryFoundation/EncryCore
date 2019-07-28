package encry.view.history

import encry.consensus.History.{Equal, Fork, HistoryComparisonResult, ModifierIds, Older, ProgressInfo, Unknown, Younger}
import encry.modifiers.history.HeaderChain
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.history.processors.ValidationError
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.{Algos, TaggedTypes}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import cats.syntax.option._
import cats.syntax.either._
import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.{ConsensusSchemeReaders, EquihashPowScheme, PowLinearController}
import encry.view.history.HistoryValidationResult.HistoryDifficultyError
import encry.view.history.processors.ValidationError.FatalValidationError.{GenesisBlockFatalValidationError, HeaderFatalValidationError}
import encry.view.history.processors.ValidationError.NonFatalValidationError.HeaderNonFatalValidationError
import org.encryfoundation.common.network.SyncInfo
import supertagged.@@

import scala.util.Try

trait HistoryExtension extends HistoryAPI {

  val powScheme: EquihashPowScheme = EquihashPowScheme(TestNetConstants.n, TestNetConstants.k)

  def payloadsToDownload(howMany: Int, excluding: HashSet[ModifierId]): (ModifierTypeId, Seq[ModifierId]) = {
    @tailrec def continuation(height: Height, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else bestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
        case Some(h) if !excluding.exists(_ sameElements h.payloadId) && !isModifierDefined(h.payloadId) =>
          continuation(Height @@ (height + 1), acc :+ h.payloadId)
        case Some(_) => continuation(Height @@ (height + 1), acc)
        case None => acc
      }

    (for {
      bestBlockId <- getBestBlockIdOpt
      headerLinkedToBestBlock <- getHeaderById(bestBlockId) //todo can be also found in cache
    } yield headerLinkedToBestBlock) match {
      //case _ if !isHeadersChainSynced => Seq.empty
      case Some(header) if isInBestChain(header) =>
        (Payload.modifierTypeId, continuation(Height @@ (header.height + 1), Seq.empty))
      case Some(header) => (Payload.modifierTypeId,
        lastBestBlockHeightRelevantToBestChain(header.height)
          .map(height => continuation(Height @@ (height + 1), Seq.empty))
          .getOrElse(continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)))
      case None =>
        (Payload.modifierTypeId, continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty))
    }
  }

  /** Checks, whether it's time to download full chain and return toDownload modifiers */
  def toDownload(header: Header): Option[(ModifierTypeId, ModifierId)] =
  // Already synced and header is not too far back. Download required modifiers
    if (header.height >= blockDownloadProcessor.minimalBlockHeight) (Payload.modifierTypeId, header.payloadId).some
    // Headers chain is synced after this header. Start downloading full blocks
    else if (!isHeadersChainSynced && isNewHeader(header)) {
      logger.info(s"Headers chain is synced after header ${header.encodedId} at height ${header.height}")
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      none[(ModifierTypeId, ModifierId)]
    } else none[(ModifierTypeId, ModifierId)]

  def getHeaderInfoUpdate(header: Header): Seq[(StorageKey, StorageValue)] = {
    //addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Seq(
        BestHeaderKey -> StorageValue @@ header.id,
        heightIdKey(TestNetConstants.GenesisHeight) -> StorageValue @@ header.id,
        modifierHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(TestNetConstants.GenesisHeight),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      )
    } else scoreOf(header.parentId).map { parentScore =>
      val score: Difficulty =
        Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
      val bestHeaderHeight: Int = getBestHeaderHeight
      val bestHeadersChainScore: BigInt = getBestHeadersChainScore
      val bestRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          Seq(BestHeaderKey -> StorageValue @@ header.id)
        else Seq.empty
      val scoreRow: (StorageKey, StorageValue) = headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
      val heightRow: (StorageKey, StorageValue) =
        modifierHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
      val headerIdsRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > bestHeaderHeight) || (header.height == bestHeaderHeight && score > bestHeadersChainScore))
          bestBlockHeaderIdsRow(header, score)
        else orphanedBlockHeaderIdsRow(header, score)
      Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow
    }.getOrElse(Seq.empty)
  }

  /** Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height */
  //todo check description
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New best header ${h.encodedId} with score: $score. New height: ${h.height}")
    val self: (StorageKey, StorageValue) =
      heightIdKey(h.height) -> StorageValue @@ (h.id :+ headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)) //todo check :+
    val forkIds: Seq[(StorageKey, StorageValue)] = getHeaderById(h.parentId)
      .toSeq.view //todo check view
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers) //todo here we need only header.id and header.height
      .filterNot(isInBestChain)
      .map(h =>
        heightIdKey(h.height) -> StorageValue @@ (h.id :+ headerIdsAtHeight(h.height).filterNot(_ sameElements h.id))
      ) //todo check :+
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  //todo check description
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdKey(h.height) -> StorageValue @@ (h.id :+ headerIdsAtHeight(h.height))) //todo check :+
  }

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  //todo check do we really need full header or can use just part of it
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec def loop(header: Header, acc: Seq[Header]): Seq[Header] =
      if (acc.length == limit || until(header)) acc
      else getHeaderById(header.parentId) match {
        case Some(parent) => loop(parent, acc :+ parent)
        case None if acc.contains(header) => acc
        case _ => acc :+ header
      }

    if (getBestHeaderIdOpt.isEmpty || (limit == 0)) HeaderChain(Seq.empty)
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  /**
    * Find first header with the best height <= $height which id satisfies condition $p
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec private def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeight(height)
    .find(id => p(id))
    .flatMap(getHeaderById) match {
    case h@Some(_) => h
    case None if height > TestNetConstants.GenesisHeight => loopHeightDown(height - 1, p)
    case n@None => n
  }



  //def addHeaderToCacheIfNecessary(h: Header): Unit

  //todo to utils object anything below

  def realDifficulty(h: Header): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)

  def requiredDifficultyAfter(parent: Header): Either[HistoryDifficultyError, Difficulty] = {
    val requiredHeights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ (parent.height + 1))
    for {
      _ <- Either.cond(requiredHeights.lastOption.contains(parent.height), (),
        HistoryDifficultyError("Incorrect heights sequence!"))
      chain = headerChainBack(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
      requiredHeaders = (requiredHeights.min to requiredHeights.max)
        .zip(chain.headers)
        .filter(p => requiredHeights.contains(p._1))
      _ <- Either.cond(requiredHeights.length == requiredHeaders.length, (),
        HistoryDifficultyError( s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}"))
      result = PowLinearController.getDifficulty(requiredHeaders)
    } yield result
  }


  /** @return all possible forks, that contains specified header */
  protected[history] def continuationHeaderChains(header: Header,
                                                  filterCond: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec
    def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextLevelHeaders: Seq[Header] = Seq(currentHeight)
        .flatMap(h => headerIdsAtHeight(h + 1))
        .flatMap(getHeaderById)
        .filter(filterCond)
      if (nextLevelHeaders.isEmpty) acc.map(_.reverse)
      else {
        val updatedChains: Seq[Seq[Header]] = nextLevelHeaders.flatMap { h =>
          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
        }
        val nonUpdatedChains: Seq[Seq[Header]] = acc.filter(chain =>
          !nextLevelHeaders.exists(_.parentId sameElements chain.head.id))
        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, Seq(Seq(header)))
  }

  def continuationIds(info: SyncInfo, size: Int): Option[ModifierIds] = Try {
    if (isEmpty) info.startingPoints
    else if (info.lastHeaderIds.isEmpty) {
      val heightFrom: Int = Math.min(bestHeaderHeight, size - 1)
      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
      //todo remove .get
      val startHeader: Header = getHeaderById(startId).get
      val headers: HeaderChain = headerChainBack(size, startHeader, _ => false)
        .ensuring(_.headers.exists(_.height == TestNetConstants.GenesisHeight),
          "Should always contain genesis header.")
      headers.headers.flatMap(h => Seq((Header.modifierTypeId, h.id)))
    } else {
      val ids: Seq[ModifierId] = info.lastHeaderIds
      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
      val theirHeight: Height = heightOf(lastHeaderInOurBestChain).get
      val heightFrom: Int = Math.min(bestHeaderHeight, theirHeight + size)
      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
      //todo remove .get
      val startHeader: Header = getHeaderById(startId).get
      headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
        .headers.map(h => Header.modifierTypeId -> h.id)
    }
  }.toOption

  /**
    * Whether another's node syncInfo shows that another node is ahead or behind ours
    *
    * @param si other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(si: SyncInfo): HistoryComparisonResult = bestHeaderIdOpt match {

    //Our best header is the same as other history best header
    case Some(id) if si.lastHeaderIds.lastOption.exists(_ sameElements id) => Equal

    //Our best header is in other history best chain, but not at the last position
    case Some(id) if si.lastHeaderIds.exists(_ sameElements id) => Older

    /* Other history is empty, or our history contains last id from other history */
    case Some(_) if si.lastHeaderIds.isEmpty || si.lastHeaderIds.lastOption.exists(isModifierDefined) => Younger

    case Some(_) =>
      //Our history contains some ids from other history
      if (si.lastHeaderIds.exists(isModifierDefined)) Fork
      //Unknown comparison result
      else Unknown

    //Both nodes do not keep any blocks
    case None if si.lastHeaderIds.isEmpty => Equal

    //Our history is empty, other contain some headers
    case None => Older
  }

  /** @return ids of count headers starting from offset */
  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
    .flatMap(h => headerIdsAtHeight(h).headOption)

  def lastHeaders(count: Int): HeaderChain = bestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, _ => false))
    .getOrElse(HeaderChain.empty)

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = historyStorage.modifiersBytesById(id)

  /**
    * Return headers, required to apply to reach header2 if you are at header1 position.
    *
    * @param fromHeaderOpt - initial position
    * @param toHeader      - header you should reach
    * @return (Modifier required to rollback first, header chain to apply)
    */
  def getChainToHeader(fromHeaderOpt: Option[Header],
                       toHeader: Header): (Option[ModifierId], HeaderChain) = fromHeaderOpt match {
    case Some(h1) =>
      val (prevChain, newChain) = commonBlockThenSuffixes(h1, toHeader)
      (prevChain.headOption.map(_.id), newChain.tail)
    case None => (None, headerChainBack(toHeader.height + 1, toHeader, _ => false))
  }

  /** Finds common block and sub-chains from common block to header1 and header2. */
  protected[history] def commonBlockThenSuffixes(header1: Header,
                                                 header2: Header): (HeaderChain, HeaderChain) = {
    val heightDelta: Int = Math.max(header1.height - header2.height, 0)

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val chains: (HeaderChain, HeaderChain) = commonBlockThenSuffixes(otherChain, header1, numberBack + heightDelta)
      if (chains._1.head == chains._2.head) chains
      else {
        val biggerOther: HeaderChain = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) loop(biggerOther.length, biggerOther)
        else throw new Exception(s"Common point not found for headers $header1 and $header2")
      }
    }

    loop(2, HeaderChain(Seq(header2)))
  }

  /** Finds common block and sub-chains with `otherChain`. */
  protected[history] def commonBlockThenSuffixes(otherChain: HeaderChain,
                                                 startHeader: Header,
                                                 limit: Int): (HeaderChain, HeaderChain) = {
    def until(h: Header): Boolean = otherChain.exists(_.id sameElements h.id)

    val currentChain: HeaderChain = headerChainBack(limit, startHeader, until)
    (currentChain, otherChain.takeAfter(currentChain.head))
  }

  def syncInfo: SyncInfo =
    if (isEmpty) SyncInfo(Seq.empty)
    else SyncInfo(bestHeaderOpt.map(header =>
      ((header.height - settings.network.maxInvObjects + 1) to header.height)
        .flatMap(height => headerIdsAtHeight(height).headOption)
    ).getOrElse(Seq.empty))

  def realDifficulty(h: Header): Difficulty

  def isNewHeader(header: Header): Boolean

  //def lastBestBlockRelevantToBestChain(atHeight: Int): Option[Block]



}

object HistoryExtension {

}