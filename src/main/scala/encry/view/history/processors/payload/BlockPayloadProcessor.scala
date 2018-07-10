package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.view.history.processors.BlockProcessor
import encry.view.history.storage.HistoryStorage

import scala.util.Try

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.modifierById(payload.headerId) match {
      case Some(header: EncryBlockHeader) =>
        historyStorage.modifierById(header.adProofsId) match {
          case _ if bestBlockIdOpt.isEmpty && !isValidFirstBlock(header) =>
            putToHistory(payload)
          case Some(adProof: ADProofs) =>
            processBlock(EncryBlock(header, payload, Some(adProof)), payloadIsNew = true)
          case None if !adState =>
            processBlock(EncryBlock(header, payload, None), payloadIsNew = true)
          case _ =>
            putToHistory(payload)
        }
      case _ =>
        throw new Exception(s"Header for modifier $payload is not defined")
    }
  }

  override protected def validate(m: EncryBlockPayload): Try[Unit] =
    modifierValidation(m, typedModifierById[EncryBlockHeader](m.headerId))

  private def putToHistory(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
