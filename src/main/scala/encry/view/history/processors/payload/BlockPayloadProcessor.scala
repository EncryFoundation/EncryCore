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

  override protected def process(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] =
    getBlockByPayload(payload).map(block => processBlock(block, payload)).getOrElse(putToHistory(payload))

  private def getBlockByPayload(payload: EncryBlockPayload): Option[EncryBlock] =
    typedModifierById[EncryBlockHeader](payload.headerId).flatMap { h =>
      if (!adState) Some(EncryBlock(h, payload, None))
      else typedModifierById[ADProofs](h.adProofsId).map(ps => EncryBlock(h, payload, Some(ps)))
    }

  override protected def validate(m: EncryBlockPayload): Try[Unit] =
    modifierValidation(m, typedModifierById[EncryBlockHeader](m.headerId))

  private def putToHistory(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
