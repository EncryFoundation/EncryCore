package encry.view.history.storage.processors

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, HistoryModifierSerializer}
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
  override def bestFullBlockIdOpt: Option[ModifierId] = historyStorage.db.get(BestFullBlockKey).map(ModifierId @@ _.data)

  protected def getFullBlock(h: EncryBlockHeader): Option[EncryBlock]

  protected def commonBlockThenSuffixes(header1: EncryBlockHeader, header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain)

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

    assert(adProofsOpt.isDefined || txsAreNew, "Only transactions can be new when proofs are empty")
    val newModRow = if (txsAreNew) {
      (ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs)))
    } else {
      (ByteArrayWrapper(adProofsOpt.get.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(adProofsOpt.get)))
    }
    val storageVersion = if (txsAreNew) txs.id else adProofsOpt.get.id
    (bestFullBlockOpt, bestFullBlockIdOpt.flatMap(scoreOf), scoreOf(header.id)) match {
      case (Some(pevBest), _, Some(score)) if header.parentId sameElements pevBest.header.id =>
        log.info(s"New best full block with header ${header.encodedId}. Height = ${header.height}, score = $score")
        if (nodeSettings.blocksToKeep >= 0) pruneOnNewBestBlock(header)
        bestBlockToTheEnd(newModRow, storageVersion, fullBlock)
      //TODO currentScore == prevBestScore
      case (Some(prevBest), Some(prevBestScore), Some(score)) if score > prevBestScore =>
        log.info(s"Process fork for new best full block with header ${header.encodedId}. " +
          s"Height = ${header.height}, score = $score")
        historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(fullBlock.header.id))))
        val (prevChain, newChain) = commonBlockThenSuffixes(prevBest.header, header)

        //todo: is flatMap in next two lines safe?
        val toRemove: Seq[EncryBlock] = prevChain.tail.headers.flatMap(getFullBlock)
        val toApply: Seq[EncryBlock] = newChain.tail.headers.flatMap(getFullBlock)
        assert(toRemove.nonEmpty, s"Should always have blocks to remove. Current = $header, prevBest = $prevBest")
        assert(toApply.nonEmpty, s"Should always have blocks to apply. Current = $header, prevBest = $prevBest")
        if (nodeSettings.blocksToKeep >= 0) {
          val bestHeight: Int = heightOf(toApply.last.header.id).get
          lazy val toClean = (bestHeight - nodeSettings.blocksToKeep - toApply.length) until
            (bestHeight - nodeSettings.blocksToKeep)
          if (bestHeight > nodeSettings.blocksToKeep) pruneBlockDataAt(toClean)
        }
        //TODO toApply?
        ProgressInfo(Some(getFullBlock(prevChain.head).get.id), toRemove, toApply.headOption, Seq())
      case (None, _, _) if nodeSettings.blocksToKeep < 0 && header.isGenesis=>
        log.info(s"Initialize full block chain with genesis header ${header.encodedId} with transactions and proofs")
        bestBlockToTheEnd(newModRow, storageVersion, fullBlock)
      case (None, _, _) if nodeSettings.blocksToKeep >= 0 =>
        log.info(s"Initialize full block chain with new best header ${header.encodedId} with transactions and proofs")
        bestBlockToTheEnd(newModRow, storageVersion, fullBlock)
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
    val id: ModifierId = ModifierId @@ Algos.hash(heights.flatMap(_.toString.getBytes).toArray)
    val toRemove: Seq[ByteArrayWrapper] = heights.flatMap(h => headerIdsAtHeight(h))
      .flatMap { id => typedModifierById[EncryBlockHeader](id) }
      .flatMap { h =>
        Seq(ByteArrayWrapper(h.adProofsId), ByteArrayWrapper(h.payloadId))
      }
    historyStorage.update(id, toRemove, Seq())
  }

  private def bestBlockToTheEnd(newModRow: (ByteArrayWrapper, ByteArrayWrapper),
                                storageVersion: ModifierId,
                                fullBlock: EncryBlock): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insert(storageVersion, Seq(newModRow, (BestFullBlockKey, ByteArrayWrapper(fullBlock.header.id))))
    ProgressInfo(None, Seq(), Some(fullBlock), Seq())
  }
}
