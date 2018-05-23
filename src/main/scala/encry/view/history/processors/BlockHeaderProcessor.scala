package encry.view.history.processors

import com.google.common.primitives.{Ints, Longs}
import encry.EncryApp
import encry.consensus._
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.settings.Constants._
import encry.settings.{Algos, Constants, NodeSettings}
import encry.view.history.Height
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait BlockHeaderProcessor extends DownloadProcessor with ScorexLogging {

  protected val nodeSettings: NodeSettings

  protected val timeProvider: NetworkTimeProvider

  private val chainParams = Constants.Chain

  private val difficultyController = PowLinearController

  val powScheme: ConsensusScheme = new EquihashPowScheme(Constants.Equihash.n, Constants.Equihash.k)

  protected val BestHeaderKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(DigestLength)(EncryBlockHeader.modifierTypeId))

  protected val BestBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(DigestLength)(-1))

  protected val historyStorage: HistoryStorage

  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T]

  def realDifficulty(h: EncryBlockHeader): Difficulty = Difficulty @@ powScheme.realDifficulty(h)

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.get(BestHeaderKey).map(ModifierId @@ _)

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  protected def headerScoreKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("score".getBytes(Algos.charset) ++ id))

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes(Algos.charset) ++ id))

  protected def validityKey(id: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("validity".getBytes(Algos.charset) ++ id))

  // Defined if `scorex.core.consensus.HistoryReader`.
  def contains(id: ModifierId): Boolean

  def bestBlockOpt: Option[EncryBlock]

  /**
    * Id of best header with transactions and proofs. None in regime that do not process transactions
    */
  def bestBlockIdOpt: Option[ModifierId]

  /**
    * @return height of best header
    */
  def bestHeaderHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(Constants.Chain.PreGenesisHeight)

  /**
    * @return height of best header with transactions and proofs
    */
  def bestBlockHeight: Int = bestBlockIdOpt.flatMap(id => heightOf(id)).getOrElse(Constants.Chain.PreGenesisHeight)

  /**
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(h: EncryBlockHeader): ProgressInfo[EncryPersistentModifier] = {
    val dataToUpdate: (Seq[(ByteArrayWrapper, ByteArrayWrapper)], EncryPersistentModifier) = getHeaderInfoUpdate(h)

    historyStorage.bulkInsert(ByteArrayWrapper(h.id), dataToUpdate._1, Seq(dataToUpdate._2))

    bestHeaderIdOpt match {
      case Some(bestHeaderId) =>
        // If we verify transactions, we don't need to send this header to state.
        // If we don't and this is the best header, we should send this header to state to update state root hash
        val toProcess = if (nodeSettings.verifyTransactions || !(bestHeaderId sameElements h.id)) Seq.empty else Seq(h)
        ProgressInfo(None, Seq.empty, toProcess, toDownload(h))
      case None =>
        log.error("Should always have best header after header application")
        EncryApp.forceStopApplication()
    }
  }

  private def getHeaderInfoUpdate(h: EncryBlockHeader): (Seq[(ByteArrayWrapper, ByteArrayWrapper)], EncryPersistentModifier) = {
    val difficulty: NBits = h.nBits
    if (h.isGenesis) {
      log.info(s"Initialize header chain with genesis header ${h.encodedId}")
      (Seq(
        BestHeaderKey -> ByteArrayWrapper(h.id),
        heightIdsKey(chainParams.GenesisHeight) -> ByteArrayWrapper(h.id),
        headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(chainParams.GenesisHeight)),
        headerScoreKey(h.id) -> ByteArrayWrapper(Longs.toByteArray(difficulty))), h)
    } else {
      val score = Difficulty @@ (scoreOf(h.parentId).get + difficulty)
      val bestRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
        if (score > bestHeadersChainScore) Seq(BestHeaderKey -> ByteArrayWrapper(h.id)) else Seq.empty
      val scoreRow = headerScoreKey(h.id) -> ByteArrayWrapper(score.toByteArray)
      val heightRow = headerHeightKey(h.id) -> ByteArrayWrapper(Ints.toByteArray(h.height))
      val headerIdsRow = if (score > bestHeadersChainScore) {
        bestBlockHeaderIdsRow(h, score)
      } else {
        orphanedBlockHeaderIdsRow(h, score)
      }
      (Seq(scoreRow, heightRow) ++ bestRow ++ headerIdsRow, h)
    }
  }

  /**
    * Update header ids to ensure, that this block id and ids of all parent blocks are in the first position of
    * header ids at this height
    */
  private def bestBlockHeaderIdsRow(h: EncryBlockHeader, score: Difficulty) = {
    val prevHeight = bestHeaderHeight
    log.info(s"New best header ${h.encodedId} with score: $score. New height: ${h.height}, old height: $prevHeight")
    val self: (ByteArrayWrapper, ByteArrayWrapper) =
      heightIdsKey(h.height) -> ByteArrayWrapper((Seq(h.id) ++ headerIdsAtHeight(h.height)).flatten.toArray)
    val parentHeaderOpt: Option[EncryBlockHeader] = typedModifierById[EncryBlockHeader](h.parentId)
    val forkHeaders = parentHeaderOpt.toSeq
      .flatMap(parent => headerChainBack(h.height, parent, h => isInBestChain(h)).headers)
      .filter(h => !isInBestChain(h))
    val forkIds: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = forkHeaders.map { header =>
      val otherIds = headerIdsAtHeight(header.height).filter(id => !(id sameElements header.id))
      heightIdsKey(header.height) -> ByteArrayWrapper((Seq(header.id) ++ otherIds).flatten.toArray)
    }
    forkIds :+ self
  }

  /**
    * Row to storage, that put this orphaned block id to the end of header ids at this height
    */
  private def orphanedBlockHeaderIdsRow(h: EncryBlockHeader, score: Difficulty) = {
    log.info(s"New orphaned header ${h.encodedId} at height ${h.height} with score $score")
    Seq(heightIdsKey(h.height) -> ByteArrayWrapper((headerIdsAtHeight(h.height) :+ h.id).flatten.toArray))
  }

  protected def validate(header: EncryBlockHeader): Try[Unit] = {
    val validator = new BlockHeaderValidator
    validator.validate(header)
  }

  protected def reportInvalid(header: EncryBlockHeader): (Seq[ByteArrayWrapper], Seq[(ByteArrayWrapper, ByteArrayWrapper)]) = {
    val modifierId = header.id
    val payloadModifiers = Seq(header.payloadId, header.adProofsId).filter(id => historyStorage.containsObject(id))
      .map(id => ByteArrayWrapper(id))

    val toRemove = Seq(headerScoreKey(modifierId), ByteArrayWrapper(modifierId)) ++ payloadModifiers
    val bestHeaderKeyUpdate = if (bestHeaderIdOpt.exists(_ sameElements modifierId)) {
      Seq(BestHeaderKey -> ByteArrayWrapper(header.parentId))
    } else Seq()
    val bestFullBlockKeyUpdate = if (bestBlockIdOpt.exists(_ sameElements modifierId)) {
      Seq(BestBlockKey -> ByteArrayWrapper(header.parentId))
    } else Seq()
    (toRemove, bestFullBlockKeyUpdate ++ bestHeaderKeyUpdate)
  }

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h))
    .exists(_ sameElements id)

  def isInBestChain(h: EncryBlockHeader): Boolean = bestHeaderIdAtHeight(h.height).exists(_ sameElements h.id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  private def bestHeadersChainScore: BigInt = scoreOf(bestHeaderIdOpt.get).get

  protected def scoreOf(id: ModifierId): Option[BigInt] = historyStorage.get(headerScoreKey(id)).map(d => BigInt(d))

  def heightOf(id: ModifierId): Option[Height] = historyStorage.get(headerHeightKey(id))
    .map(d => Height @@ Ints.fromByteArray(d))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
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
  protected def headerChainBack(limit: Int,
                                startHeader: EncryBlockHeader,
                                until: EncryBlockHeader => Boolean): EncryHeaderChain = {
    @tailrec
    def loop(header: EncryBlockHeader, acc: Seq[EncryBlockHeader]): Seq[EncryBlockHeader] = {
      if (acc.length == limit || until(header)) {
        acc
      } else {
        typedModifierById[EncryBlockHeader](header.parentId) match {
          case Some(parent: EncryBlockHeader) =>
            loop(parent, acc :+ parent)
          case None if acc.contains(header) =>
            acc
          case _ =>
            acc :+ header
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) EncryHeaderChain(Seq())
    else EncryHeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  /**
    * Find first header with the best height <= $height which id satisfies condition $p
    * @param height - start height
    * @param p - condition to satisfy
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

  def requiredDifficultyAfter(parent: EncryBlockHeader): NBits = {
    val parentHeight = heightOf(parent.id).get
    if (parentHeight <= 2) chainParams.InitialNBits
    else {
      val requiredHeights =
        difficultyController.getHeightsForRetargetingAt(Height @@ (parentHeight + 1))
          .ensuring(_.last == parentHeight, "Incorrect heights sequence!")
      val chain = headerChainBack(requiredHeights.max - requiredHeights.min + 1,
        parent, (_: EncryBlockHeader) => false)
      val requiredHeaders = (requiredHeights.min to requiredHeights.max)
        .zip(chain.headers).filter(p => requiredHeights.contains(p._1))
      assert(requiredHeights.length == requiredHeaders.length,
        s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}")
      difficultyController.getDifficulty(requiredHeaders)
    }
  }

  class BlockHeaderValidator {

    type ValidationResult = Try[Unit]

    def validate(header: EncryBlockHeader): ValidationResult = {
      lazy val parentOpt = typedModifierById[EncryBlockHeader](header.parentId)
      if (header.parentId sameElements EncryBlockHeader.GenesisParentId) {
        if (bestHeaderIdOpt.nonEmpty) {
          Failure(new Exception("Trying to append genesis block to non-empty history."))
        } else if (header.height != chainParams.GenesisHeight) {
          Failure(new Exception("Invalid height for genesis block header."))
        } else {
          Success()
        }
      } else if (parentOpt.isEmpty) {
        Failure(new Exception(s"Parental header <id: ${Algos.encode(header.parentId)}> does not exist!"))
      } else if (header.height != parentOpt.get.height + 1) {
        Failure(new Exception(s"Invalid height in header <id: ${header.id}>"))
      } else if (header.timestamp - timeProvider.time() > Constants.Chain.MaxTimeDrift) {
        Failure(new Exception(s"Invalid timestamp in header <id: ${header.id}>"))
      } else if (header.timestamp < parentOpt.get.timestamp) {
        Failure(new Exception("Header timestamp is less than parental`s"))
      } else if (realDifficulty(header) < header.requiredDifficulty) {
        Failure(new Exception("Header <id: ${header.id}> difficulty too low."))
      } else if (!heightOf(header.parentId).exists(h => bestHeaderHeight - h < chainParams.MaxRollback)) {
        Failure(new Exception("Header is too old to be applied."))
      } else if (!header.validSignature) {
        Failure(new Exception("Block signature is invalid."))
      } else {
        Success()
      }.recoverWith { case exc =>
        log.warn("Validation error: ", exc)
        Failure(exc)
      }
    }
  }
}
