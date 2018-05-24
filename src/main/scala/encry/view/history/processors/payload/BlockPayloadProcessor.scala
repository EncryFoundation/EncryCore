package encry.view.history.processors.payload

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.view.history.processors.BlockProcessor
import encry.view.history.storage.HistoryStorage
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(txs: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.modifierById(txs.headerId) match {
      case Some(header: EncryBlockHeader) =>
        historyStorage.modifierById(header.adProofsId) match {
          case _ if bestBlockIdOpt.isEmpty && !isValidFirstBlock(header) =>
            putToHistory(txs)
          case Some(adProof: ADProofs) =>
            processBlock(EncryBlock(header, txs, Some(adProof)), payloadIsNew = true)
          case None if !adState =>
            processBlock(EncryBlock(header, txs, None), payloadIsNew = true)
          case _ =>
            putToHistory(txs)
        }
      case _ =>
        throw new Exception(s"Header for modifier $txs is no defined")
    }
  }

  override protected def validate(m: EncryBlockPayload): Try[Unit] =
    modifierValidation(m, typedModifierById[EncryBlockHeader](m.headerId))

  private def putToHistory(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
