package encry.view.history.processors

import encry.consensus.Difficulty
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

trait BlockProcessor extends BlockHeaderProcessor with ScorexLogging {

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestBlockIdOpt: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  protected def getBlock(header: EncryBlockHeader): Option[EncryBlock]

  protected def commonBlockThenSuffixes(header1: EncryBlockHeader,
                                        header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain)

  protected[history] def continuationHeaderChains(header: EncryBlockHeader,
                                                  filterCond: EncryBlockHeader => Boolean): Seq[Seq[EncryBlockHeader]]

  /**
    * Processes EncryBlock.
    *
    * @param block - block to process
    * @param isNewerPayload - flag, that blockPayload where added last
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processBlock(block: EncryBlock,
                             isNewerPayload: Boolean): ProgressInfo[EncryPersistentModifier] = {

    val newModRow = getModRowFromBlock(block, isNewerPayload)
    val storageVersion = ByteArrayWrapper(if (isNewerPayload) block.payload.id else block.adProofsOpt.get.id)
    val bestFullChain = getBestFullChain(block.header)
    val bestHeaderNew = bestFullChain.last

    processFirstBlock(block, newModRow, bestHeaderNew.id, storageVersion, bestBlockOpt)
      .orElse(processBetterChain(block, newModRow, bestHeaderNew.id, storageVersion))
        .getOrElse(nonBestBlock(block, newModRow, bestHeaderNew.id, storageVersion))
  }

  private def processFirstBlock(block: EncryBlock,
                                newModRow: EncryPersistentModifier,
                                newBestHeader: ModifierId,
                                storageVersion: ByteArrayWrapper,
                                bestBlockOpt: Option[EncryBlock]): Option[ProgressInfo[EncryPersistentModifier]] =
    if (isValidFirstFullBlock(block.header)) updateStorage(newModRow, storageVersion, block, newBestHeader)
    else None

  private def processBetterChain(block: EncryBlock,
                                 newModRow: EncryPersistentModifier,
                                 newBestHeaderId: ModifierId,
                                 storageVersion: ByteArrayWrapper): Option[ProgressInfo[EncryPersistentModifier]] =
      for {
        prevBestFullBlock <- bestBlockOpt
        bestFullBlockId <- bestBlockIdOpt
        prevBestScore <- scoreOf(bestFullBlockId)
        score <- scoreOf(newBestHeaderId)
        if score > prevBestScore
      } yield applyBetterChain(block, newModRow, prevBestFullBlock, newBestHeaderId, storageVersion)

  private def applyBetterChain(block: EncryBlock,
                               newModRow: EncryPersistentModifier,
                               prevBestFullBlock: EncryBlock,
                               newBestHeaderId: ModifierId,
                               storageVersion: ByteArrayWrapper): ProgressInfo[EncryPersistentModifier] = {
    val (prevChain, newChain) = commonBlockThenSuffixes(prevBestFullBlock.header, block.header)
    val toRemove: Seq[EncryBlock] = prevChain.tail.headers.flatMap(getBlock)
    val toApply: Seq[EncryBlock] = newChain.tail.headers
      .flatMap(h => if(h == block.header) Some(block) else getBlock(h))

    if (toApply.lengthCompare(newChain.length - 1) == 0) {
      log.info(s"Process fork for new best full block with header ${block.header.encodedId}. " +
        s"Height = ${block.header.height}, score = ${scoreOf(newBestHeaderId)}")
      nonBestBlock(block, newModRow, newBestHeaderId, storageVersion)
    } else {

      val branchPoint = toRemove.headOption.map(_ => prevChain.head.id)

      updateStorage(newModRow, storageVersion, block, newBestHeaderId)

      if (nodeSettings.blocksToKeep >= 0) {
        val bestHeight: Int = block.header.height
        val diff = block.header.height - prevBestFullBlock.header.height
        val lastKept = bestHeight - nodeSettings.blocksToKeep
        clipHistoryDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
      }
      ProgressInfo(branchPoint, toRemove, toApply.headOption, Seq.empty)
    }
  }

  private def nonBestBlock(block: EncryBlock,
                           newModRow: EncryPersistentModifier,
                           newBestHeaderId: ModifierId,
                           storageVersion: ByteArrayWrapper): ProgressInfo[EncryPersistentModifier] = {
    //Orphaned block or full chain is not initialized yet
    updateStorage(newModRow, storageVersion, block, newBestHeaderId).get
  }

  protected def isValidFirstFullBlock(header: EncryBlockHeader): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeightVar && bestBlockOpt.isEmpty

  private def getModRowFromBlock(block: EncryBlock, isNewerPayload: Boolean): EncryPersistentModifier =
    if(isNewerPayload) block.payload
    else block.adProofsOpt.getOrElse(throw new Error("Only block payload can be new when proofs are empty"))

  private def getBestFullChain(header: EncryBlockHeader): Seq[EncryBlockHeader] =
    continuationHeaderChains(header, _ => true)
      .map(_.tail)
      .map(hc => hc.map(getBlock).takeWhile(_.isDefined).flatten.map(_.header))
      .map(c => header +: c)
      .maxBy(c => c.last.difficulty.untag(Difficulty))

  private def clipHistoryDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[EncryBlockHeader](id))
      .flatMap(h => Seq(h.adProofsId, h.payloadId))
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(newModRow: EncryPersistentModifier,
                            storageVersion: ByteArrayWrapper,
                            toApply: EncryBlock,
                            bestFullHeaderId: ModifierId): Option[ProgressInfo[EncryPersistentModifier]] = {
    historyStorage.bulkInsert(storageVersion, Seq((BestBlockKey, ByteArrayWrapper(bestFullHeaderId))), Seq(newModRow))
    Option(ProgressInfo(None, Seq.empty, Some(toApply), Seq.empty))
  }

  protected def modifierValidation(m: EncryPersistentModifier,
                                   headerOpt: Option[EncryBlockHeader]): Try[Unit] = {
    if (historyStorage.containsObject(m.id)) {
      Failure(new Error(s"Modifier $m is already in history"))
    } else {
      val minimalHeight = blockDownloadProcessor.minimalBlockHeight
      headerOpt match {
        case None =>
          Failure(new Error(s"Header for modifier $m is not defined"))
        case Some(header: EncryBlockHeader) if header.height < minimalHeight =>
          Failure(new Error(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight"))
        case Some(header: EncryBlockHeader) if !header.isRelated(m) =>
          Failure(new Error(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}"))
        case Some(_) =>
          Success()
      }
    }
  }
}
