package encry.view.history.processors

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.forceStopApplication
import encry.consensus.History.ProgressInfo
import encry.consensus._
import encry.modifiers.history._
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbValue
import encry.utils.NetworkTimeProvider
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity
import scorex.crypto.hash.Digest32
import supertagged.@@
import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.HashSet
import encry.view.history.processors.ValidationError.FatalValidationError._
import encry.view.history.processors.ValidationError.NonFatalValidationError._
import cats.syntax.either._

trait BlockHeaderProcessor extends StrictLogging { //scalastyle:ignore

  protected val settings: EncryAppSettings
  protected val timeProvider: NetworkTimeProvider
  private val difficultyController: PowLinearController.type = PowLinearController
  val powScheme: EquihashPowScheme = EquihashPowScheme(TestNetConstants.n, TestNetConstants.k)
  protected val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(TestNetConstants.DigestLength)(Header.modifierTypeId.untag(ModifierTypeId))
  protected val BestBlockKey: StorageKey = StorageKey @@ Array.fill(TestNetConstants.DigestLength)(-1: Byte)
  protected val historyStorage: HistoryStorage
  lazy val blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor(settings.node)
  private var isHeadersChainSyncedVar: Boolean = false
  var headersForSyncInfo: IndexedSeq[ModifierId] = IndexedSeq.empty[ModifierId]

  protected def getBlock(h: Header): Option[Block]

  /**
    * headersCacheIndexes & headersCache contains headers from (currentBestHeaderHeight - maxRollBackDepth) to
    * headersCacheIndexes contains info about heights & headers ids
    * headersCache contains all header with ids from headersCacheIndexes
    */

  var headersCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var lastAppliedHeadersCache: Map[ByteArrayWrapper, Header] = Map.empty[ByteArrayWrapper, Header]

  var blockHeadersCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var lastAppliedBlockHeadersCache: Map[ByteArrayWrapper, Header] = Map.empty[ByteArrayWrapper, Header]

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet.
    * Transactions for this Header may be missed, to get block from best full chain (in mode that support
    * it) call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap(typedModifierById[Header])

  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  def modifiersToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Height, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else {
        headerIdsAtHeight(height).headOption
          .flatMap(id => lastAppliedHeadersCache.get(ByteArrayWrapper(id)).orElse(typedModifierById[Header](id))) match {
          case Some(bestHeaderAtThisHeight) =>
            logger.info(s"requiredModifiersForHeader($bestHeaderAtThisHeight) ->" +
              s"${requiredModifiersForHeader(bestHeaderAtThisHeight).map(x => Algos.encode(x._2))}")
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)

            val b = toDownload.filter(m => !excluding.exists(_ sameElements m._2))

            val c = b.filter(m => !contains(m._2)) //todo can we combine this 2 filter?

            continuation(Height @@ (height + 1), acc ++ c)
          case None => acc
        }
      }

    logger.info(s"BEST BLOCK OPT ${bestBlockOpt.map(x => Algos.encode(x.id))}")
    bestBlockOpt match {
      case _ if !isHeadersChainSynced =>
        Seq.empty
      case Some(fb) =>
        //if best block in best chain, just process all blocks after that
        if (isInBestChain(fb.header)) continuation(Height @@ (fb.header.height + 1), Seq.empty)
        //if not, we should find last full block from best chain, and start processing all blocks after that
        else {
          val lastFullBlock = lastBestBlockRelevantToBestChain(fb.header.height)
          logger.info(s"Last full block in best chain is block: ${lastFullBlock.map(block => Algos.encode(block.id))}")
          lastFullBlock.map(block => continuation(Height @@ (block.header.height + 1), Seq.empty))
            .getOrElse(continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty))
        }
      case None =>
        continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
    }
  }

  def lastBestBlockRelevantToBestChain(atHeight: Int): Option[Block] = {
    val blocksAtHeight: Option[Block] = for {
      headerId <- bestHeaderIdAtHeight(atHeight)
      header <- typedModifierById[Header](headerId)
      block <- getBlock(header)
    } yield block
    blocksAtHeight.orElse(lastBestBlockRelevantToBestChain(atHeight - 1))
  }


  def modifiersToDownloadForNVH(howMany: Int): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Height, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else {
        headerIdsAtHeight(height).headOption.flatMap(id =>
          lastAppliedHeadersCache.get(ByteArrayWrapper(id)).orElse(typedModifierById[Header](id))) match {
          case Some(bestHeaderAtThisHeight) =>
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => !contains(m._2))
            continuation(Height @@ (height + 1), acc ++ toDownload)
          case None => acc
        }
      }

    bestBlockOpt match {
      case _ if !isHeadersChainSynced => Seq.empty
      case Some(fb) => continuation(Height @@ (fb.header.height + 1), Seq.empty)
      case None => continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
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

  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] =
    Seq((Payload.modifierTypeId, h.payloadId))

  private def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      TestNetConstants.DesiredBlockInterval.toMillis * TestNetConstants.NewHeaderTimeMultiplier

  def typedModifierById[T <: PersistentModifier](id: ModifierId): Option[T]

  def realDifficulty(h: Header): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.get(BestHeaderKey).map(ModifierId @@ _)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  private def heightIdsKey(height: Int): StorageKey = StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)

  protected def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)

  protected def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)

  protected def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)

  def contains(id: ModifierId): Boolean

  def bestBlockOpt: Option[Block]

  def bestBlockIdOpt: Option[ModifierId]

  def bestHeaderHeight: Int = bestHeaderOpt.map(_.height).getOrElse(TestNetConstants.PreGenesisHeight)

  def bestBlockHeight: Int = bestBlockOpt.map(_.header.height).getOrElse(TestNetConstants.PreGenesisHeight)

  protected def process(h: Header): ProgressInfo[PersistentModifier] = getHeaderInfoUpdate(h) match {
    case Some(dataToUpdate) =>
      historyStorage.bulkInsert(h.id, dataToUpdate._1, Seq(dataToUpdate._2))
      bestHeaderIdOpt match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[Header] =
            if (!(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logger.error("Should always have best header after header application")
          forceStopApplication()
      }
    case None => ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def addHeaderToCacheIfNecessary(h: Header): Unit =
    if (h.height >= bestHeaderHeight - TestNetConstants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(h.id)} to header cache")
      val newHeadersIdsAtHeaderHeight = headersCacheIndexes.getOrElse(h.height, Seq.empty[ModifierId]) :+ h.id
      headersCacheIndexes = headersCacheIndexes + (h.height -> newHeadersIdsAtHeaderHeight)
      lastAppliedHeadersCache = lastAppliedHeadersCache + (ByteArrayWrapper(h.id) -> h)
      // cleanup cache if necessary
      if (headersCacheIndexes.size > TestNetConstants.MaxRollbackDepth) {
        headersCacheIndexes.get(bestHeaderHeight - TestNetConstants.MaxRollbackDepth).foreach { headersIds =>
          val wrappedIds = headersIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup header cache from headers: ${headersIds.map(Algos.encode).mkString(",")}")
          lastAppliedHeadersCache = lastAppliedHeadersCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        headersCacheIndexes = headersCacheIndexes - (bestHeaderHeight - TestNetConstants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${lastAppliedHeadersCache.size}")
      logger.debug(s"headersCacheIndexes size: ${headersCacheIndexes.size}")
    }

  private def addBlockToCacheIfNecessary(b: Block): Unit =
    if (b.header.height >= bestBlockHeight - TestNetConstants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(b.header.id)} to header cache")
      val newHeadersIdsAtHeaderHeight = blockHeadersCacheIndexes.getOrElse(b.header.height, Seq.empty[ModifierId]) :+ b.header.id
      blockHeadersCacheIndexes = blockHeadersCacheIndexes + (b.header.height -> newHeadersIdsAtHeaderHeight)
      lastAppliedBlockHeadersCache = lastAppliedBlockHeadersCache + (ByteArrayWrapper(b.header.id) -> b.header)
      // cleanup cache if necessary
      if (blockHeadersCacheIndexes.size > TestNetConstants.MaxRollbackDepth) {
        blockHeadersCacheIndexes.get(bestHeaderHeight - TestNetConstants.MaxRollbackDepth).foreach { headersIds =>
          val wrappedIds = headersIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup header cache from headers: ${headersIds.map(Algos.encode).mkString(",")}")
          lastAppliedBlockHeadersCache = lastAppliedBlockHeadersCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        blockHeadersCacheIndexes = blockHeadersCacheIndexes - (bestHeaderHeight - TestNetConstants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${lastAppliedBlockHeadersCache.size}")
      logger.debug(s"blockHeadersCacheIndexes size: ${blockHeadersCacheIndexes.size}")
    }

  private def getHeaderInfoUpdate(header: Header): Option[(Seq[(StorageKey, StorageValue)], PersistentModifier)] = {
    addHeaderToCacheIfNecessary(header)
    if (header.isGenesis) {
      logger.info(s"Initialize header chain with genesis header ${header.encodedId}")
      Option(Seq(
        BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId),
        heightIdsKey(TestNetConstants.GenesisHeight) -> StorageValue @@ header.id.untag(ModifierId),
        headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(TestNetConstants.GenesisHeight),
        headerScoreKey(header.id) -> StorageValue @@ header.difficulty.toByteArray) -> header)
    } else {
      scoreOf(header.parentId).map { parentScore =>
        val score: BigInt @@ Difficulty.Tag =
          Difficulty @@ (parentScore + ConsensusSchemeReaders.consensusScheme.realDifficulty(header))
        val bestRow: Seq[(StorageKey, StorageValue)] =
          if ((header.height > bestHeaderHeight) ||
            (header.height == bestHeaderHeight && score > bestHeadersChainScore)) {
            Seq(BestHeaderKey -> StorageValue @@ header.id.untag(ModifierId))
          } else Seq.empty
        val scoreRow: (StorageKey, StorageValue) =
          headerScoreKey(header.id) -> StorageValue @@ score.toByteArray
        val heightRow: (StorageKey, StorageValue) =
          headerHeightKey(header.id) -> StorageValue @@ Ints.toByteArray(header.height)
        val headerIdsRow: Seq[(StorageKey, StorageValue)] =
          if ((header.height > bestHeaderHeight) ||
            (header.height == bestHeaderHeight && score > bestHeadersChainScore)) {
            val row = bestBlockHeaderIdsRow(header, score)
            row
          }
          else orphanedBlockHeaderIdsRow(header, score)
        (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, header)
      }
    }
  }


  /** Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    val prevHeight = bestHeaderHeight
    logger.info(s"New best header ${h.encodedId} with score: $score." +
      s" New height: ${h.height}, old height: $prevHeight")
    val self: (StorageKey, StorageValue) =
      heightIdsKey(h.height) -> StorageValue @@ (Seq(h.id) ++
        headerIdsAtHeight(h.height).filterNot(_ sameElements h.id)
        ).flatten.toArray
    val parentHeaderOpt: Option[Header] =
      lastAppliedHeadersCache.get(ByteArrayWrapper(h.parentId)).orElse(typedModifierById[Header](h.parentId))
    val forkHeaders: Seq[Header] = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(StorageKey, StorageValue)] = forkHeaders.map { header =>
      val otherIds: Seq[ModifierId] = headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)
      heightIdsKey(header.height) -> StorageValue @@ (Seq(header.id) ++ otherIds).flatten.toArray
    }
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(StorageKey, StorageValue)] = {
    logger.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> StorageValue @@ (headerIdsAtHeight(h.height) :+ h.id).flatten.toArray)
  }

  protected def validate(h: Header): Either[ValidationError, Header] =
    if (h.isGenesis) HeaderValidator.validateGenesisBlockHeader(h)
    else lastAppliedHeadersCache
      .get(ByteArrayWrapper(h.parentId))
      .orElse(typedModifierById[Header](h.parentId))
      .map(p => HeaderValidator.validateHeader(h, p))
      .getOrElse(HeaderNonFatalValidationError(s"Header's ${h.encodedId} parent doesn't contain in history").asLeft[Header])

  def isInBestChain(id: ModifierId): Boolean = heightOf(id)
    .flatMap(h => bestHeaderIdAtHeight(h))
    .exists(_.sameElements(id))

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).exists(_ sameElements h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  protected def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.get(headerScoreKey(id)).map(d => BigInt(d))

  def heightOf(id: ModifierId): Option[Height] = historyStorage
    .get(headerHeightKey(id))
    .map(d => Height @@ Ints.fromByteArray(d))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] = historyStorage.store
    .get(heightIdsKey(height))
    .map { elem => elem.untag(VersionalLevelDbValue).grouped(32).map(ModifierId @@ _).toSeq }
    .getOrElse(Seq.empty[ModifierId])

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    logger.info(s"limit: $limit. startHeader: ${startHeader.height}")
    @tailrec
    def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.length == limit || until(header)) acc
      else lastAppliedHeadersCache.get(ByteArrayWrapper(header.parentId))
        .orElse(lastAppliedBlockHeadersCache.get(ByteArrayWrapper(header.parentId)))
        .orElse(typedModifierById[Header](header.parentId)) match {
          case Some(parent: Header) => loop(parent, acc :+ parent)
          case None if acc.contains(header) => acc
          case _ => acc :+ header
        }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) HeaderChain(Seq())
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
  protected final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = {
    headerIdsAtHeight(height).find(id => p(id))
      .flatMap(id => lastAppliedHeadersCache.get(ByteArrayWrapper(id)).orElse(typedModifierById[Header](id))) match {
      case Some(header) => Some(header)
      case None if height > TestNetConstants.GenesisHeight => loopHeightDown(height - 1, p)
      case None => None
    }
  }

  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val parentHeight: Int = parent.height
    val requiredHeights: Seq[Height] = difficultyController.getHeightsForRetargetingAt(Height @@ (parentHeight + 1))
      .ensuring(_.last == parentHeight, "Incorrect heights sequence!")
    val chain: HeaderChain = headerChainBack(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
    val requiredHeaders: immutable.IndexedSeq[(Int, Header)] = (requiredHeights.min to requiredHeights.max)
      .zip(chain.headers).filter(p => requiredHeights.contains(p._1))
    assert(requiredHeights.length == requiredHeaders.length,
      s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}")
    difficultyController.getDifficulty(requiredHeaders)
  }

  object HeaderValidator {

    def validateGenesisBlockHeader(h: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.parentId.sameElements(Header.GenesisParentId), (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} should has genesis parent id"))
      _ <- Either.cond(bestHeaderIdOpt.isEmpty, (),
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
      _ <- Either.cond(heightOf(h.parentId).exists(h => bestHeaderHeight - h < TestNetConstants.MaxRollbackDepth), (),
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