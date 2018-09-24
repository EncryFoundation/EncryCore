package encry.view.history.processors

import com.google.common.primitives.Ints
import encry.EncryApp
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.consensus.History.ProgressInfo
import encry.consensus.{ModifierSemanticValidity, _}
import encry.local.explorer.BlockListener.NewOrphaned
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history._
import encry.settings.Constants._
import encry.settings.{Constants, NodeSettings}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.{Logging, NetworkTimeProvider}
import encry.validation.{ModifierValidator, ValidationResult}
import encry.view.history.History.Height
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

trait BlockHeaderProcessor extends Logging {

  protected val nodeSettings: NodeSettings
  protected val timeProvider: NetworkTimeProvider
  private val difficultyController: PowLinearController.type = PowLinearController
  val powScheme: EquihashPowScheme = EquihashPowScheme(Constants.Equihash.n, Constants.Equihash.k)
  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(DigestLength)(Header.modifierTypeId))
  protected val BestBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(DigestLength)(-1))
  protected val historyStorage: HistoryStorage
  lazy val blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor(nodeSettings)
  private var isHeadersChainSyncedVar: Boolean = false

  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  def modifiersToDownload(howMany: Int, excluding: Iterable[ModifierId]): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Height, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
      if (acc.lengthCompare(howMany) >= 0) acc
      else {
        headerIdsAtHeight(height).headOption.flatMap(id => typedModifierById[Header](id)) match {
          case Some(bestHeaderAtThisHeight) =>
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => !excluding.exists(_ sameElements m._2))
              .filter(m => !contains(m._2))
            continuation(Height @@ (height + 1), acc ++ toDownload)
          case None => acc
        }
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
    if (!nodeSettings.verifyTransactions) Seq.empty // Regime that do not download and verify transaction
    else if (header.height >= blockDownloadProcessor.minimalBlockHeight)
      requiredModifiersForHeader(header) // Already synced and header is not too far back. Download required modifiers
    else if (!isHeadersChainSynced && isNewHeader(header)) {
      // Headers chain is synced after this header. Start downloading full blocks
      logInfo(s"Headers chain is synced after header ${header.encodedId} at height ${header.height}")
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      Seq.empty
    } else Seq.empty

  private def requiredModifiersForHeader(h: Header): Seq[(ModifierTypeId, ModifierId)] =
    if (!nodeSettings.verifyTransactions) Seq.empty
    else if (nodeSettings.stateMode.isDigest)
      Seq((Payload.modifierTypeId, h.payloadId), (ADProofs.modifierTypeId, h.adProofsId))
    else Seq((Payload.modifierTypeId, h.payloadId))

  private def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      Constants.Chain.DesiredBlockInterval.toMillis * Constants.Chain.NewHeaderTimeMultiplier

  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T]

  def realDifficulty(h: Header): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.get(BestHeaderKey).map(ModifierId @@ _)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(Algos.charset) ++ id))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(Algos.charset) ++ id))

  protected def validityKey(id: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(Algos.charset) ++ id))

  def contains(id: ModifierId): Boolean

  def bestBlockOpt: Option[Block]

  def bestBlockIdOpt: Option[ModifierId]

  def bestHeaderHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(Constants.Chain.PreGenesisHeight)

  def bestBlockHeight: Int = bestBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(Constants.Chain.PreGenesisHeight)

  protected def process(h: Header): ProgressInfo[EncryPersistentModifier] = getHeaderInfoUpdate(h) match {
    case Some(dataToUpdate) =>
      historyStorage.bulkInsert(ByteArrayWrapper(h.id), dataToUpdate._1, Seq(dataToUpdate._2))
      bestHeaderIdOpt match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[Header] =
            if (nodeSettings.verifyTransactions || !(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logError("Should always have best header after header application")
          EncryApp.forceStopApplication()
      }
    case None => ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def getHeaderInfoUpdate(h: Header): Option[(Seq[(ByteArrayWrapper, ByteArrayWrapper)],
                                                      EncryPersistentModifier)] = {
    val difficulty: Difficulty = h.difficulty
    if (h.isGenesis) {
      logInfo(s"Initialize header chain with genesis header ${h.encodedId}")
      Option(Seq(
        BestHeaderKey -> ByteArrayWrapper(h.id),
        heightIdsKey(Constants.Chain.GenesisHeight) -> ByteArrayWrapper(h.id),
        headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(Constants.Chain.GenesisHeight)),
        headerScoreKey(h.id) -> ByteArrayWrapper(difficulty.toByteArray)), h)
    } else {
      scoreOf(h.parentId).map { parentScore =>
        val score = Difficulty @@ (parentScore + difficulty)
        val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
          if (score > bestHeadersChainScore) Seq(BestHeaderKey -> ByteArrayWrapper(h.id)) else Seq.empty
        val scoreRow: (ByteArrayWrapper, ByteArrayWrapper) =
          headerScoreKey(h.id) -> ByteArrayWrapper(score.toByteArray)
        val heightRow: (ByteArrayWrapper, ByteArrayWrapper) =
          headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
        val headerIdsRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (score > bestHeadersChainScore) {
          bestBlockHeaderIdsRow(h, score)
        } else {
          EncryApp.system.actorSelection("/user/blockListener") ! NewOrphaned(h)
          orphanedBlockHeaderIdsRow(h, score)
        }
        (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, h)
      }
    }
  }

  /** Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height */
  private def bestBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val prevHeight = bestHeaderHeight
    logInfo(s"New best header ${h.encodedId} with score: $score." +
      s" New height: ${h.height}, old height: $prevHeight")
    val self: (ByteArrayWrapper, ByteArrayWrapper) =
      heightIdsKey(h.height) -> ByteArrayWrapper((Seq(h.id) ++ headerIdsAtHeight(h.height)).flatten.toArray)
    val parentHeaderOpt: Option[Header] = typedModifierById[Header](h.parentId)
    val forkHeaders: Seq[Header] = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = forkHeaders.map { header =>
      val otherIds: Seq[ModifierId] = headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)
      heightIdsKey(header.height) -> ByteArrayWrapper((Seq(header.id) ++ otherIds).flatten.toArray)
    }
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  private def orphanedBlockHeaderIdsRow(h: Header, score: Difficulty): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    logInfo(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatten.toArray))
  }

  protected def validate(header: Header): Try[Unit] = HeaderValidator.validate(header).toTry

  protected def reportInvalid(header: Header): (Seq[ByteArrayWrapper], Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = {
    val payloadModifiers: Seq[ByteArrayWrapper] = Seq(header.payloadId, header.adProofsId)
      .filter(id => historyStorage.containsObject(id))
      .map(id => ByteArrayWrapper(id))
    val toRemove: Seq[ByteArrayWrapper] = Seq(headerScoreKey(header.id), ByteArrayWrapper(header.id)) ++ payloadModifiers
    val bestHeaderKeyUpdate: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (bestHeaderIdOpt.exists(_ sameElements header.id)) Seq(BestHeaderKey -> ByteArrayWrapper(header.parentId))
      else Seq()
    val bestFullBlockKeyUpdate: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (bestBlockIdOpt.exists(_ sameElements header.id)) Seq(BestBlockKey -> ByteArrayWrapper(header.parentId))
      else Seq()
    (toRemove, bestFullBlockKeyUpdate ++ bestHeaderKeyUpdate)
  }

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h))
    .exists(_ sameElements id)

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).exists(_ sameElements h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  protected def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.get(headerScoreKey(id)).map(d => BigInt(d))

  def heightOf(id: ModifierId): Option[Height] = historyStorage.get(headerHeightKey(id))
    .map(d => Height @@ Ints.fromByteArray(d))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    ModifierId @@ historyStorage.store.get(heightIdsKey(height: Int)).map(_.data).getOrElse(Array()).grouped(32).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  protected def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec
    def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.length == limit || until(header)) acc
      else typedModifierById[Header](header.parentId) match {
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
    headerIdsAtHeight(height).find(id => p(id)).flatMap(id => typedModifierById[Header](id)) match {
      case Some(header) => Some(header)
      case None if height > Constants.Chain.GenesisHeight => loopHeightDown(height - 1, p)
      case None => None
    }
  }

  def requiredDifficultyAfter(parent: Header): Difficulty = {
    val parentHeight: Block.Height = parent.height
    val requiredHeights: Seq[Height] =
      difficultyController.getHeightsForRetargetingAt(Height @@ (parentHeight + 1))
        .ensuring(_.last == parentHeight, "Incorrect heights sequence!")
    val chain: HeaderChain = headerChainBack(requiredHeights.max - requiredHeights.min + 1,
      parent, (_: Header) => false)
    val requiredHeaders: immutable.IndexedSeq[(Int, Header)] = (requiredHeights.min to requiredHeights.max)
      .zip(chain.headers).filter(p => requiredHeights.contains(p._1))
    assert(requiredHeights.length == requiredHeaders.length,
      s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}")
    difficultyController.getDifficulty(requiredHeaders)
  }

  object HeaderValidator extends ModifierValidator {

    def validate(header: Header): ValidationResult =
      if (header.isGenesis) validateGenesisBlockHeader(header)
      else typedModifierById[Header](header.parentId).map { parent =>
        validateChildBlockHeader(header, parent)
      } getOrElse error(s"Parent header with id ${Algos.encode(header.parentId)} is not defined")

    private def validateGenesisBlockHeader(header: Header): ValidationResult =
      accumulateErrors
        .validateEqualIds(header.parentId, Header.GenesisParentId) { detail =>
          fatal(s"Genesis block should have genesis parent id. $detail")
        }
        .validate(bestHeaderIdOpt.isEmpty) {
          fatal("Trying to append genesis block to non-empty history")
        }
        .validate(header.height == Constants.Chain.GenesisHeight) {
          fatal(s"Height of genesis block $header is incorrect")
        }
        .result

    private def validateChildBlockHeader(header: Header, parent: Header): ValidationResult = {
      failFast
        .validate(header.timestamp - timeProvider.estimatedTime <= Constants.Chain.MaxTimeDrift) {
          error(s"Header timestamp ${header.timestamp} is too far in future from now " +
            s"${timeProvider.estimatedTime}")
        }
        .validate(header.timestamp > parent.timestamp) {
          fatal(s"Header timestamp ${header.timestamp} is not greater than parents ${parent.timestamp}")
        }
        .validate(header.height == parent.height + 1) {
          fatal(s"Header height ${header.height} is not greater by 1 than parents ${parent.height}")
        }
        .validateNot(historyStorage.containsObject(header.id)) {
          fatal("Header is already in history")
        }
        .validate(realDifficulty(header) >= header.requiredDifficulty) {
          fatal(s"Block difficulty ${realDifficulty(header)} is less than required " +
            s"${header.requiredDifficulty}")
        }
        .validate(header.difficulty >= requiredDifficultyAfter(parent)){
          fatal(s"Incorrect required difficulty in header: " +
            s"${Algos.encode(header.id)} on height ${header.height}")
        }
        .validate(heightOf(header.parentId).exists(h => bestHeaderHeight - h < Constants.Chain.MaxRollbackDepth)) {
        fatal(s"Trying to apply too old block difficulty at height ${heightOf(header.parentId)}")
      }
        .validate(powScheme.verify(header)) {
          fatal(s"Wrong proof-of-work solution for $header")
        }
        .validateSemantics(isSemanticallyValid(header.parentId)) {
          fatal("Parent header is marked as semantically invalid")
        }.result
    }
  }

}