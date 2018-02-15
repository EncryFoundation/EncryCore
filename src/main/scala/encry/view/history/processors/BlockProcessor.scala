package encry.view.history.processors

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait BlockProcessor extends BlockHeaderProcessor with ScorexLogging {

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestFullBlockIdOpt: Option[ModifierId] = historyStorage.get(BestFullBlockKey).map(ModifierId @@ _)

  protected def getFullBlock(h: EncryBlockHeader): Option[EncryBlock]

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
    val header: EncryBlockHeader = block.header
    val blockPayload: EncryBlockPayload = block.payload
    val adProofsOpt: Option[ADProofs] = block.adProofsOpt
      .ensuring(_.isDefined || isNewerPayload, "Only block payload can be new when proofs are empty")
    val newModRow = if (isNewerPayload) blockPayload else adProofsOpt.get
    val storageVersion = ByteArrayWrapper(if (isNewerPayload) blockPayload.id else adProofsOpt.get.id)
    val continuations = continuationHeaderChains(header, _ => true).map(_.tail)
    val bestFullChain = continuations.map(hc => hc.map(getFullBlock).takeWhile(_.isDefined).flatten.map(_.header))
      .map(c => header +: c)
      .maxBy(c => scoreOf(c.last.id))

    val bestHeaderNew = bestFullChain.last

    (bestFullBlockOpt, bestFullBlockIdOpt.flatMap(scoreOf), scoreOf(bestHeaderNew.id)) match {
      case (None, _, _) if header.isGenesis =>
        log.info(s"Initialize block chain with genesis header ${bestHeaderNew.encodedId} with transactions and proofs")
        updateStorage(newModRow, storageVersion, block, bestHeaderNew.id)

      case (Some(prevBest), _, Some(score)) if header.parentId sameElements prevBest.header.id =>
        log.info(s"New best full block with header ${bestHeaderNew.encodedId}. " +
          s"Height = ${bestHeaderNew.height}, score = $score")
        if (nodeSettings.blocksToKeep >= 0) clipHistoryOnNewBestBlock(header)
        updateStorage(newModRow, storageVersion, block, bestHeaderNew.id)

      case (Some(prevBest), Some(prevBestScore), Some(score)) if score > prevBestScore =>
        val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, header)
        val toRemove: Seq[EncryBlock] = prevChain.tail.headers.flatMap(getFullBlock)
        val toApply: Seq[EncryBlock] = newChain.tail.headers
          .flatMap(h => if(h == block.header) Some(block) else getFullBlock(h))

        if (toApply.lengthCompare(newChain.length - 1) == 0) {
          log.info(s"Process fork for new best full block with header ${bestHeaderNew.encodedId}. " +
            s"Height = ${bestHeaderNew.height}, score = $score")
          updateStorage(newModRow, storageVersion, block, bestHeaderNew.id)

          if (nodeSettings.blocksToKeep >= 0) {
            val bestHeight: Int = bestHeaderNew.height
            val diff = bestHeaderNew.height - prevBest.header.height
            val lastKept = bestHeight - nodeSettings.blocksToKeep
            clipHistoryDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
          }
          ProgressInfo(Some(prevChain.head.id), toRemove, toApply.headOption, Seq.empty)
        } else {
          log.info(s"Got transactions and proofs for header ${header.encodedId} with no connection to genesis")
          historyStorage.bulkInsert(storageVersion, Seq.empty, Seq(newModRow))
          ProgressInfo(None, Seq.empty, None, Seq.empty)
        }

      // TODO: currentScore == prevBestScore

      case _ =>
        log.info(s"Got transactions and proofs for non-best header ${header.encodedId}")
        historyStorage.bulkInsert(storageVersion, Seq.empty, Seq(newModRow))
        ProgressInfo(None, Seq.empty, None, Seq.empty)
    }
  }

  private def clipHistoryOnNewBestBlock(header: EncryBlockHeader): Unit =
    heightOf(header.id).filter(h => h > nodeSettings.blocksToKeep)
    .foreach(h => clipHistoryDataAt(Seq(h - nodeSettings.blocksToKeep)))

  private def clipHistoryDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val toRemove: Seq[ModifierId] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap { id => typedModifierById[EncryBlockHeader](id) }
      .flatMap { h =>
        Seq(h.adProofsId, h.payloadId)
      }
    historyStorage.removeObjects(toRemove)
  }

  private def updateStorage(newModRow: EncryPersistentModifier,
                            storageVersion: ByteArrayWrapper,
                            toApply: EncryBlock,
                            bestFullHeaderId: ModifierId): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.bulkInsert(storageVersion, Seq((BestFullBlockKey, ByteArrayWrapper(bestFullHeaderId))), Seq(newModRow))
    ProgressInfo(None, Seq.empty, Some(toApply), Seq.empty)
  }
}
