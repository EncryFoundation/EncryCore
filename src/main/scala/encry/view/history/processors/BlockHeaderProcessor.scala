package encry.view.history.processors

import com.google.common.primitives.Ints
import encry.EncryApp
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.consensus.History.ProgressInfo
import encry.consensus.{ModifierSemanticValidity, _}
import encry.local.explorer.BlockListener.NewOrphaned
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.history.block.{Block, EncryBlock}
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
import io.circe.syntax._
import io.circe.{Encoder, Json}
import scala.collection.immutable
import scala.util.Try

trait BlockHeaderProcessor extends Logging { //scalastyle:ignore

  protected val nodeSettings: NodeSettings
  protected val timeProvider: NetworkTimeProvider
  private val difficultyController: PowLinearController.type = PowLinearController
  val powScheme: EquihashPowScheme = EquihashPowScheme(Constants.Equihash.n, Constants.Equihash.k)
  protected val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(DigestLength)(EncryBlockHeader.modifierTypeId))
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
        headerIdsAtHeight(height).map{modifierId =>
          logInfo(s"Going to calculate necessary modifiers: ${Algos.encode(modifierId)}")
          modifierId
        }.headOption.flatMap(id => {
          logInfo(s"Header by this id is: ${typedModifierById[EncryBlockHeader](id).asJson}")
          typedModifierById[EncryBlockHeader](id)
        }) match {
          case Some(bestHeaderAtThisHeight) =>
            logInfo(s"Best header on height $height is ${bestHeaderAtThisHeight.asJson}")
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => !excluding.exists(_ sameElements m._2))
              .filter(m => !contains(m._2))
            logInfo(s"Need to download: ${toDownload.map(_._2).map(Algos.encode).mkString(",")}")
            continuation(Height @@ (height + 1), acc ++ toDownload)
          case None => acc
        }
      }
    }

    logInfo(s"Going to download modifiers. BestBlockOpt is: ${bestBlockOpt.map(_.asJson)}")

    bestBlockOpt match {
      case _ if !isHeadersChainSynced =>
        logInfo("Header chain is not sync!")
        Seq.empty
      case Some(fb) =>
        logInfo("Looks like bestBlockOpt exist!")
        continuation(Height @@ fb.header.height, Seq.empty)
      case None => continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
    }
  }

  /** Checks, whether it's time to download full chain and return toDownload modifiers */
  protected def toDownload(header: EncryBlockHeader): Seq[(ModifierTypeId, ModifierId)] =
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

  private def requiredModifiersForHeader(h: EncryBlockHeader): Seq[(ModifierTypeId, ModifierId)] =
    if (!nodeSettings.verifyTransactions) Seq.empty
    else if (nodeSettings.stateMode.isDigest)
      Seq((EncryBlockPayload.modifierTypeId, h.payloadId), (ADProofs.modifierTypeId, h.adProofsId))
    else Seq((EncryBlockPayload.modifierTypeId, h.payloadId))

  private def isNewHeader(header: EncryBlockHeader): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      Constants.Chain.DesiredBlockInterval.toMillis * Constants.Chain.NewHeaderTimeMultiplier

  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T]

  def realDifficulty(h: EncryBlockHeader): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)

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

  def bestBlockOpt: Option[EncryBlock]

  def bestBlockIdOpt: Option[ModifierId]

  def bestHeaderHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(Constants.Chain.PreGenesisHeight)

  def bestBlockHeight: Int = bestBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(Constants.Chain.PreGenesisHeight)

  protected def process(h: EncryBlockHeader): ProgressInfo[EncryPersistentModifier] = getHeaderInfoUpdate(h) match {
    case Some(dataToUpdate) =>
      historyStorage.bulkInsert(ByteArrayWrapper(h.id), dataToUpdate._1, Seq(dataToUpdate._2))
      bestHeaderIdOpt match {
        case Some(bestHeaderId) =>
          val toProcess: Seq[EncryBlockHeader] =
            if (nodeSettings.verifyTransactions || !(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
          ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
        case None =>
          logError("Should always have best header after header application")
          EncryApp.forceStopApplication()
      }
    case None => ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }

  private def getHeaderInfoUpdate(h: EncryBlockHeader):
  Option[(Seq[(ByteArrayWrapper, ByteArrayWrapper)], EncryPersistentModifier)] = {
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
        val score: Difficulty = Difficulty @@ (parentScore + difficulty)
        val betterScore: Boolean = bestHeaderIdOpt
          .flatMap(scoreOf)
          .exists(_ < score)
        val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
          if (betterScore) Seq(BestHeaderKey -> ByteArrayWrapper(h.id)) else Seq.empty
        val scoreRow: (ByteArrayWrapper, ByteArrayWrapper) = headerScoreKey(h.id) -> ByteArrayWrapper(score.toByteArray)
        val heightRow: (ByteArrayWrapper, ByteArrayWrapper) =
          headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
        val headerIdsRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = if (betterScore) {
          logInfo(s"New best header ${h.encodedId} with score: $score. New height: ${h.height}, " +
            s"old height: $bestHeaderHeight")
          bestBlockHeaderIdsRow(h)
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
  private def bestBlockHeaderIdsRow(h: EncryBlockHeader): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    val self: (ByteArrayWrapper, ByteArrayWrapper) =
      heightIdsKey(h.height) -> ByteArrayWrapper((Seq(h.id) ++ headerIdsAtHeight(h.height)).flatten.toArray)
    val parentHeaderOpt: Option[EncryBlockHeader] = typedModifierById[EncryBlockHeader](h.parentId)
    val forkHeaders: Seq[EncryBlockHeader] = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = forkHeaders.map { header =>
      val otherIds: Seq[ModifierId] = headerIdsAtHeight(header.height).filterNot(_ sameElements header.id)
      heightIdsKey(header.height) -> ByteArrayWrapper((Seq(header.id) ++ otherIds).flatten.toArray)
    }
    forkIds :+ self
  }

  /** Row to storage, that put this orphaned block id to the end of header ids at this height */
  private def orphanedBlockHeaderIdsRow(h: EncryBlockHeader, score: Difficulty): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    logInfo(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatten.toArray))
  }

  protected def validate(header: EncryBlockHeader): Try[Unit] = HeaderValidator.validate(header).toTry

  protected def reportInvalid(header: EncryBlockHeader): (Seq[ByteArrayWrapper], Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = {
    val payloadModifiers: Seq[ByteArrayWrapper] =
      Seq(header.payloadId, header.adProofsId).filter(id => historyStorage.containsObject(id))
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

  def isInBestChain(h: EncryBlockHeader): Boolean = bestHeaderIdAtHeight(h.height).exists(_ sameElements h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

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
  protected def headerChainBack(limit: Int, startHeader: EncryBlockHeader,
                                until: EncryBlockHeader => Boolean): EncryHeaderChain = {
    @tailrec
    def loop(header: EncryBlockHeader, acc: Seq[EncryBlockHeader]): Seq[EncryBlockHeader] = {
      if (acc.length == limit || until(header)) acc
      else typedModifierById[EncryBlockHeader](header.parentId) match {
        case Some(parent: EncryBlockHeader) => loop(parent, acc :+ parent)
        case None if acc.contains(header) => acc
        case _ => acc :+ header
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) EncryHeaderChain(Seq())
    else EncryHeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  /**
    * Find first header with the best height <= $height which id satisfies condition $p
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec
  protected final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[EncryBlockHeader] = {
    headerIdsAtHeight(height).find(id => p(id)).flatMap(id => typedModifierById[EncryBlockHeader](id)) match {
      case Some(header) => Some(header)
      case None if height > Constants.Chain.GenesisHeight => loopHeightDown(height - 1, p)
      case None => None
    }
  }

  def requiredDifficultyAfter(parent: EncryBlockHeader): Difficulty = {
    val parentHeight: Block.Height = parent.height
    val requiredHeights: Seq[Height] =
      difficultyController.getHeightsForRetargetingAt(Height @@ (parentHeight + 1))
        .ensuring(_.last == parentHeight, "Incorrect heights sequence!")
    val chain: EncryHeaderChain = headerChainBack(requiredHeights.max - requiredHeights.min + 1,
      parent, (_: EncryBlockHeader) => false)
    val requiredHeaders: immutable.IndexedSeq[(Int, EncryBlockHeader)] = (requiredHeights.min to requiredHeights.max)
      .zip(chain.headers).filter(p => requiredHeights.contains(p._1))
    assert(requiredHeights.length == requiredHeaders.length,
      s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}")
    difficultyController.getDifficulty(requiredHeaders)
  }

  object HeaderValidator extends ModifierValidator {

    def validate(header: EncryBlockHeader): ValidationResult =
      if (header.isGenesis) validateGenesisBlockHeader(header)
      else typedModifierById[EncryBlockHeader](header.parentId).map { parent =>
        validateChildBlockHeader(header, parent)
      } getOrElse error(s"Parent header with id ${Algos.encode(header.parentId)} is not defined")

    private def validateGenesisBlockHeader(header: EncryBlockHeader): ValidationResult =
      accumulateErrors
        .validateEqualIds(header.parentId, EncryBlockHeader.GenesisParentId) { detail =>
          fatal(s"Genesis block should have genesis parent id. $detail")
        }
        .validate(bestHeaderIdOpt.isEmpty) {
          fatal("Trying to append genesis block to non-empty history")
        }
        .validate(header.height == Constants.Chain.GenesisHeight) {
          fatal(s"Height of genesis block $header is incorrect")
        }
        .result

    private def validateChildBlockHeader(header: EncryBlockHeader, parent: EncryBlockHeader): ValidationResult = {
      failFast
        .validate(header.timestamp - timeProvider.estimatedTime <= Constants.Chain.MaxTimeDrift) {
          error(s"Header timestamp ${header.timestamp} is too far in future from now ${timeProvider.estimatedTime}")
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
          fatal(s"Block difficulty ${realDifficulty(header)} is less than required ${header.requiredDifficulty}")
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