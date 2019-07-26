package encry.view.history

import encry.consensus.History.ProgressInfo
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
import supertagged.@@

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

  def processHeader(h: Header): ProgressInfo[PersistentModifier] = getHeaderInfoUpdate(h) match {
    case dataToUpdate: Seq[_] if dataToUpdate.nonEmpty =>
      history.bulkInsert(h.id, dataToUpdate, Seq(h)) //side effect
      getBestHeaderIdOpt match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[Header] = if (!(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logger.error("Should always have best header after header application")
          forceStopApplication() //todo possibly remove this
      }
    case _ => ProgressInfo(None, Seq.empty, Seq.empty, none)
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
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
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
  //todo check do we realy need full header or can use just part of it
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

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    history.get(validityKey(modifierId)) match {
      case Some(mod) if mod.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(mod) if mod.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if isModifierDefined(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case mod => logger.error(s"Incorrect validity status: $mod")
        ModifierSemanticValidity.Absent
    }

  def validate(h: Header): Either[ValidationError, Header] =
    if (h.isGenesis) HeadersValidator.validateGenesisBlockHeader(h)
    else getHeaderById(h.parentId)
      .map(p => HeadersValidator.validateHeader(h, p))
      .getOrElse(HeaderNonFatalValidationError(s"Header's ${h.encodedId} parent doesn't contain in history").asLeft[Header])

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

  def realDifficulty(h: Header): Difficulty

  def isNewHeader(header: Header): Boolean

  //def lastBestBlockRelevantToBestChain(atHeight: Int): Option[Block]

  object HeadersValidator {
    def validateGenesisBlockHeader(h: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.parentId.sameElements(Header.GenesisParentId), (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} should has genesis parent id"))
      _ <- Either.cond(getBestHeaderIdOpt.isEmpty, (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} appended to non-empty history"))
      _ <- Either.cond(h.height == TestNetConstants.GenesisHeight, (),
        GenesisBlockFatalValidationError(s"Height of genesis block with header ${h.encodedId} is incorrect"))
    } yield h

    def validateHeader(h: Header, parent: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.timestamp > parent.timestamp, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has timestamp ${h.timestamp} less than parent's ${parent.timestamp}"))
      _ <- Either.cond(h.height == parent.height + 1, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has height ${h.height} not greater by 1 than parent's ${parent.height}"))
      _ <- Either.cond(!history.containsMod(h.id), (),
        HeaderFatalValidationError(s"Header ${h.encodedId} is already in history"))
      _ <- Either.cond(realDifficulty(h) >= h.requiredDifficulty, (),
        HeaderFatalValidationError(s"Incorrect real difficulty in header ${h.encodedId}"))

      _ <- Either.cond(h.difficulty >= requiredDifficultyAfter(parent).getOrElse(0), (),
        HeaderFatalValidationError(s"Incorrect required difficulty in header ${h.encodedId}"))
      _ <- Either.cond(heightOf(h.parentId).exists(h => getBestHeaderHeight - h < TestNetConstants.MaxRollbackDepth), (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has height greater than max roll back depth"))
      powSchemeValidationResult = powScheme.verify(h)
      _ <- Either.cond(powSchemeValidationResult.isRight, (),
        HeaderFatalValidationError(s"Wrong proof-of-work solution in header ${h.encodedId} caused: $powSchemeValidationResult"))
      _ <- Either.cond(isSemanticallyValid(h.parentId) != ModifierSemanticValidity.Invalid, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} is semantically invalid"))
      _ <- Either.cond(h.timestamp - timeProvider.estimatedTime <= TestNetConstants.MaxTimeDrift, (),
        HeaderNonFatalValidationError(s"Header ${h.encodedId} with timestamp ${h.timestamp} is too far in future from now ${timeProvider.estimatedTime}"))
    } yield h
  }

}

object HistoryExtension {

}