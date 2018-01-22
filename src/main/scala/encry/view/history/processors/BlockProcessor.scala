package encry.view.history.processors

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings.Algos
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.consensus.History.ProgressInfo
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait BlockProcessor extends BlockHeaderProcessor with ScorexLogging {

  /**
    * Id of header that contains transactions and proofs
    */
  override def bestFullBlockIdOpt: Option[ModifierId] =
    historyStorage.db.get(BestFullBlockKey).map(ModifierId @@ _.data)

  protected def getFullBlock(h: EncryBlockHeader): Option[EncryBlock]

  protected def commonBlockThenSuffixes(header1: EncryBlockHeader,
                                        header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain)

  protected[history] def continuationHeaderChains(header: EncryBlockHeader): Seq[EncryHeaderChain]

  /**
    * Process full block when we have one.
    *
    * @param fullBlock - block to process
    * @param txsAreNew - flag, that transactions where added last
    * @return ProgressInfo required for State to process to be consistent with History
    */
  protected def processFullBlock(fullBlock: EncryBlock,
                                 txsAreNew: Boolean): ProgressInfo[EncryPersistentModifier] = {
    val header: EncryBlockHeader = fullBlock.header
    val txs: EncryBlockPayload = fullBlock.payload
     val adProofsOpt: Option[ADProofs] = fullBlock.adProofsOpt
      .ensuring(_.isDefined || txsAreNew, "Only transactions can be new when proofs are empty")

    val newModRow = if (txsAreNew) {
      (ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs)))
    } else {
      (ByteArrayWrapper(adProofsOpt.get.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(adProofsOpt.get)))
    }
    val storageVersion = if (txsAreNew) txs.id else adProofsOpt.get.id

    val continuations = continuationHeaderChains(header).map(_.headers.tail)
    val bestFullChain = continuations.map(hc => hc.map(getFullBlock).takeWhile(_.isDefined).flatten.map(_.header))
      .map(c => header +: c)
      .maxBy(c => scoreOf(c.last.id))

    val bestChainNew = bestFullChain.last

    (bestFullBlockOpt, bestFullBlockIdOpt.flatMap(scoreOf), scoreOf(bestChainNew.id)) match {
      case (None, _, _) if nodeSettings.blocksToKeep < 0 && header.isGenesis =>
        log.info(s"Initialize full block chain with genesis header ${header.encodedId} with transactions and proofs")
        updateStorage(newModRow, storageVersion, fullBlock, fullBlock.header.id)
      case (None, _, _) if nodeSettings.blocksToKeep >= 0 =>
        log.info(s"Initialize full block chain with new best header ${header.encodedId} with transactions and proofs")
        updateStorage(newModRow, storageVersion, fullBlock, fullBlock.header.id)
      case (Some(prevBest), _, Some(score)) if header.parentId sameElements prevBest.header.id =>
        log.info(s"New best full block with header ${bestChainNew.encodedId}. " +
          s"Height = ${bestChainNew.height}, score = $score")
        if (nodeSettings.blocksToKeep >= 0) pruneOnNewBestBlock(header)
        updateStorage(newModRow, storageVersion, fullBlock, bestChainNew.id)

      case (Some(prevBest), Some(prevBestScore), Some(score)) if score > prevBestScore =>
        //TODO currentScore == prevBestScore
        val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, header)
        val toRemove: Seq[EncryBlock] = prevChain.tail.headers.flatMap(getFullBlock)
        if(toRemove.nonEmpty) {
          log.info(s"Process fork for new best full block with header ${bestChainNew.encodedId}. " +
            s"Height = ${bestChainNew.height}, score = $score")
          updateStorage(newModRow, storageVersion, fullBlock, bestChainNew.id)

          if (nodeSettings.blocksToKeep >= 0) {
            val bestHeight: Int = bestChainNew.height
            val diff = bestChainNew.height - prevBest.header.height
            val lastKept = bestHeight - nodeSettings.blocksToKeep
            pruneBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
          }
          ProgressInfo(Some(getFullBlock(prevChain.head).get.id), toRemove, Some(getFullBlock(newChain(1)).get), Seq())
        } else {
          log.info(s"Got transactions and proofs for header ${header.encodedId} with no connection to genesis")
          historyStorage.insert(storageVersion, Seq(newModRow))
          ProgressInfo(None, Seq(), None, Seq())
        }
      case _ =>
        log.info(s"Got transactions and proofs for non-best header ${header.encodedId}")
        historyStorage.insert(storageVersion, Seq(newModRow))
        ProgressInfo(None, Seq(), None, Seq())
    }
  }

  private def pruneOnNewBestBlock(header: EncryBlockHeader): Unit =
    heightOf(header.id).filter(h => h > nodeSettings.blocksToKeep)
    .foreach(h => pruneBlockDataAt(Seq(h - nodeSettings.blocksToKeep)))

  private def pruneBlockDataAt(heights: Seq[Int]): Try[Unit] = Try {
    val mid: ModifierId = ModifierId @@ Algos.hash(heights.flatMap(_.toString.getBytes).toArray)
    val toRemove: Seq[ByteArrayWrapper] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap(id => typedModifierById[EncryBlockHeader](id))
      .flatMap(h => Seq(ByteArrayWrapper(h.adProofsId), ByteArrayWrapper(h.payloadId)))
    historyStorage.update(mid, toRemove, Seq())
  }

  private def updateStorage(newModRow: (ByteArrayWrapper, ByteArrayWrapper),
                            storageVersion: ModifierId,
                            toApply: EncryBlock,
                            bestFullHeaderId: ModifierId): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(bestFullHeaderId))))
    ProgressInfo(None, Seq(), Some(toApply), Seq())
  }
}
