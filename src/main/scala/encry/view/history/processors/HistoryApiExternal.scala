package encry.view.history.processors

import com.google.common.primitives.Ints
import encry.EncryApp.forceStopApplication
import encry.consensus.History.ProgressInfo
import encry.consensus._
import encry.modifiers.history._
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.utils.NetworkTimeProvider
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity
import supertagged.@@
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import encry.view.history.processors.ValidationError.FatalValidationError._
import encry.view.history.processors.ValidationError.NonFatalValidationError._
import cats.syntax.either._

trait HistoryApiExternal extends HistoryApiInternal {

  val settings: EncryAppSettings

  val timeProvider: NetworkTimeProvider

  var headersCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var headersCache: Map[ByteArrayWrapper, Header] = Map.empty[ByteArrayWrapper, Header]

  var blocksCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var blocksCache: Map[ByteArrayWrapper, Block] = Map.empty[ByteArrayWrapper, Block]

  lazy val blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor(settings.node)

  def getHeaderById(id: ModifierId): Option[Header] = headersCache
    .get(ByteArrayWrapper(id))
    .orElse(getHeaderByIdInternal(id))
  def getBlockByHeader(header: Header): Option[Block] = blocksCache
    .get(ByteArrayWrapper(header.id))
    .orElse(getPayloadByIdInternal(header.payloadId).map(p => Block(header, p)))
  def getBlockByHeaderId(id: ModifierId): Option[Block] = blocksCache
    .get(ByteArrayWrapper(id))
    .orElse(getHeaderById(id).flatMap(h => getPayloadByIdInternal(h.payloadId).map(p => Block(h, p))))

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else getBestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
        case Some(h) if !excluding.exists(_.sameElements(h.payloadId)) && !isModifierDefined(h.payloadId) =>
          continuation(height + 1, acc :+ h.payloadId)
        case Some(_) =>
          continuation(height + 1, acc)
        case None =>
          acc
      }

    (for {
      bestBlockId             <- getBestBlockId
      headerLinkedToBestBlock <- getHeaderById(bestBlockId)
    } yield headerLinkedToBestBlock) match {
        case _ if !isHeadersChainSynced =>
          Seq.empty
        case Some(header) if isInBestChain(header) =>
          continuation(header.height + 1, Seq.empty)
        case Some(header) =>
          lastBestBlockHeightRelevantToBestChain(header.height)
            .map(height => continuation(height + 1, Seq.empty))
            .getOrElse(continuation(blockDownloadProcessor.minimalBlockHeightVar, Seq.empty))
        case None =>
          continuation(blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
    }
  }

  /** Checks, whether it's time to download full chain and return toDownload modifiers */
  protected def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] =
    if (header.height >= blockDownloadProcessor.minimalBlockHeight)
      requiredModifiersForHeader(header) // Already synced and header is not too far back. Download required modifiers
    else if (!isHeadersChainSynced && isNewHeader(header)) {
      // Headers chain is synced after this header. Start downloading full blocks
      logger.info(s"Headers chain is synced after header ${header.encodedId} at height ${header.height}")
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      Seq.empty
    } else Seq.empty

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.length == limit || until(header)) acc
      else getHeaderById(header.parentId) match {
        case Some(parent: Header) => loop(parent, acc :+ parent)
        case None if acc.contains(header) => acc
        case _ => acc :+ header
      }
    }

    if (getBestHeaderId.isEmpty || (limit == 0)) HeaderChain(Seq())
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }


  /**
    * Find first header with the best height <= $height which id satisfies condition $p
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec
  protected final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeight(height)
    .find(id => p(id))
    .flatMap(getHeaderById) match {
    case Some(header) => Some(header)
    case None if height > TestNetConstants.GenesisHeight => loopHeightDown(height - 1, p)
    case None => None
  }


  //todo remove asset + ensuring
  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val requiredHeights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ (parent.height + 1))
      .ensuring(_.last == parent.height, "Incorrect heights sequence!")
    val chain: HeaderChain = headerChainBack(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
    val requiredHeaders: IndexedSeq[(Int, Header)] = (requiredHeights.min to requiredHeights.max)
      .zip(chain.headers)
      .filter(p => requiredHeights.contains(p._1))
    assert(requiredHeights.length == requiredHeaders.length,
      s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}")
    PowLinearController.getDifficulty(requiredHeaders)
  }


  val powScheme: EquihashPowScheme = EquihashPowScheme(TestNetConstants.n, TestNetConstants.k)

  private var isHeadersChainSyncedVar: Boolean = false

  def realDifficulty(h: Header): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)

  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar


  def lastBestBlockRelevantToBestChain(atHeight: Int): Option[Block] = {
    val bestBlockAtHeight: Option[Block] = for {
      headerId <- getBestHeaderIdAtHeight(atHeight)
      header <- getHeaderById(headerId)
      block <- getBlockByHeader(header)
    } yield block
    bestBlockAtHeight.orElse(lastBestBlockRelevantToBestChain(atHeight - 1))
  }


  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] =
    Seq((Payload.modifierTypeId, h.payloadId))

  private def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      TestNetConstants.DesiredBlockInterval.toMillis * TestNetConstants.NewHeaderTimeMultiplier


  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity


  protected def process(h: Header): ProgressInfo[PersistentModifier] = getHeaderInfoUpdate(h) match {
    case Some(dataToUpdate) =>
      historyStorage.bulkInsert(h.id, dataToUpdate._1, Seq(dataToUpdate._2))
      getBestHeaderId match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[Header] = if (!(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logger.error("Should always have best header after header application")
          forceStopApplication()
      }
    case None => ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def addHeaderToCacheIfNecessary(h: Header): Unit =
    if (h.height >= getBestHeaderHeight - TestNetConstants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(h.id)} to header cache")
      val newHeadersIdsAtHeaderHeight = headersCacheIndexes.getOrElse(h.height, Seq.empty[ModifierId]) :+ h.id
      headersCacheIndexes = headersCacheIndexes + (h.height -> newHeadersIdsAtHeaderHeight)
      headersCache = headersCache + (ByteArrayWrapper(h.id) -> h)
      // cleanup cache if necessary
      if (headersCacheIndexes.size > TestNetConstants.MaxRollbackDepth) {
        headersCacheIndexes.get(getBestHeaderHeight - TestNetConstants.MaxRollbackDepth).foreach { headersIds =>
          val wrappedIds = headersIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup header cache from headers: ${headersIds.map(Algos.encode).mkString(",")}")
          headersCache = headersCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        headersCacheIndexes = headersCacheIndexes - (getBestHeaderHeight - TestNetConstants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${headersCache.size}")
      logger.debug(s"headersCacheIndexes size: ${headersCacheIndexes.size}")
    }

  private def getHeaderInfoUpdate(header: Header): Option[(Seq[(StorageKey, StorageValue)], PersistentModifier)] = {
    addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Option(Seq(
        BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId),
        heightIdsKey(TestNetConstants.GenesisHeight) -> StorageValue @@ header.id.untag(ModifierId),
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(TestNetConstants.GenesisHeight),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray
      ) -> header)
    } else scoreOf(header.parentId).map { parentScore =>
      val score: BigInt @@ Difficulty.Tag =
        Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
      val bestRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > getBestHeaderHeight) ||
          (header.height == getBestHeaderHeight && score > getBestHeadersChainScore)) {
          Seq(BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId))
        } else Seq.empty
      val scoreRow: (StorageKey, StorageValue) =
        headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
      val heightRow: (StorageKey, StorageValue) =
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
      val headerIdsRow: Seq[(StorageKey, StorageValue)] =
        if ((header.height > getBestHeaderHeight) || (header.height == getBestHeaderHeight && score > getBestHeadersChainScore))
          bestBlockHeaderIdsRow(header, score)
        else orphanedBlockHeaderIdsRow(header, score)
      (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, header)
    }
  }


  /** Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    val prevHeight: Int = getBestHeaderHeight
    logger.info(s"New best header ${h.encodedId} with score: $score." +
      s" New height: ${h.height}, old height: $prevHeight")
    val self: (StorageKey, StorageValue) =
      heightIdsKey(h.height) ->
        StorageValue @@ (Seq(h.id) ++ headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)).flatten.toArray
    val parentHeaderOpt: Option[Header] = getHeaderById(h.parentId)
    val forkHeaders: Seq[Header] = parentHeaderOpt
      .toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(StorageKey, StorageValue)] = forkHeaders.map { header =>
      val otherIds: Seq[ModifierId] = headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)
      heightIdsKey(header.height) -> StorageValue @@ (Seq(header.id) ++ otherIds).flatten.toArray
    }
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  //todo why return type is Seq instead tuple2?
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> StorageValue @@ (headerIdsAtHeight(h.height) :+ h.id).flatten.toArray)
  }

  protected def validate(h: Header): Either[ValidationError, Header] =
    if (h.isGenesis) HeaderValidator.validateGenesisBlockHeader(h)
    else getHeaderById(h.parentId)
      .map(p => HeaderValidator.validateHeader(h, p))
      .getOrElse(HeaderNonFatalValidationError(s"Header's ${h.encodedId} parent doesn't contain in history").asLeft[Header])


  object HeaderValidator {

    def validateGenesisBlockHeader(h: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.parentId.sameElements(Header.GenesisParentId), (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} should has genesis parent id"))
      _ <- Either.cond(getBestHeaderId.isEmpty, (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} appended to non-empty history"))
      _ <- Either.cond(h.height == TestNetConstants.GenesisHeight, (),
        GenesisBlockFatalValidationError(s"Height of genesis block with header ${h.encodedId} is incorrect"))
    } yield h

    def validateHeader(h: Header, parent: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.timestamp > parent.timestamp, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has timestamp ${h.timestamp} less than parent's ${parent.timestamp}"))
      _ <- Either.cond(h.height == parent.height + 1, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has height ${h.height} not greater by 1 than parent's ${parent.height}"))
      _ <- Either.cond(!historyStorage.containsObject(h.id), (),
        HeaderFatalValidationError(s"Header ${h.encodedId} is already in history"))
      _ <- Either.cond(realDifficulty(h) >= h.requiredDifficulty, (),
        HeaderFatalValidationError(s"Incorrect real difficulty in header ${h.encodedId}"))
      _ <- Either.cond(h.difficulty >= requiredDifficultyAfter(parent), (),
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