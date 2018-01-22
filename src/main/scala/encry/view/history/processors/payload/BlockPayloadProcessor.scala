package encry.view.history.processors.payload

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.{ADProofs, HistoryModifierSerializer}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.view.history.storage.HistoryStorage
import encry.view.history.processors.BlockProcessor
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(txs: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.modifierById(txs.headerId) match {
      case Some(header: EncryBlockHeader) =>
        historyStorage.modifierById(header.adProofsId) match {
          case Some(adProof: ADProofs) =>
            processFullBlock(new EncryBlock(header, txs, Some(adProof)), txsAreNew = true)
          case None if !adState =>
            processFullBlock(new EncryBlock(header, txs, None), txsAreNew = true)
          case _ =>
            val modifierRow = Seq((ByteArrayWrapper(txs.id), ByteArrayWrapper(HistoryModifierSerializer.toBytes(txs))))
            historyStorage.insert(txs.id, modifierRow)
            ProgressInfo(None, Seq(), None, Seq())
        }
      case _ =>
        throw new Error(s"Header for modifier $txs is no defined")
    }
  }

  override protected def validate(m: EncryBlockPayload): Try[Unit] = Try {

    if (historyStorage.contains(m.id))
      Failure(new Error(s"Modifier $m is already in history"))

    historyStorage.modifierById(m.headerId) match {
      case Some(header: EncryBlockHeader) =>
        if (!(header.txsRoot sameElements m.digest))
          Failure(new Error(s"Header transactions root ${Base58.encode(header.txsRoot)} differs from block transactions $m digest"))

        bestFullBlockIdOpt match {
          case None if nodeSettings.blocksToKeep < 0 =>
            if (!header.isGenesis)
              Failure(new Error("Trying to apply non-genesis block to empty history in fullnode mode"))
          case None if nodeSettings.blocksToKeep >= 0 =>
          // TODO: State should be at this version!
          case Some(_) =>
            // TODO: `contains()` is unimplemented.
            if (!typedModifierById[EncryBlockHeader](header.parentId).exists(h => contains(h.payloadId)))
              Failure(new Error("Trying to apply transactions for header, which parent transactions are empty"))
        }

      case _ =>
        throw new Error(s"Header for modifier $m does not exist")
    }
  }
}
