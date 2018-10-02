package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, Block, Header, Payload}
import encry.view.history.processors.BlockProcessor
import encry.view.history.storage.HistoryStorage
import encry.EncryApp.settings.network

import scala.util.Try

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(payload: Payload): ProgressInfo[EncryPersistentModifier] =
    getBlockByPayload(payload)
      .flatMap { block =>
        if (block.header.height - bestHeaderHeight >= 2 + network.maxInvObjects) None
        else Some(processBlock(block, payload))
      }.getOrElse(putToHistory(payload))

  private def getBlockByPayload(payload: Payload): Option[Block] =
    typedModifierById[Header](payload.headerId).flatMap { h =>
      if (!adState) Some(Block(h, payload, None))
      else typedModifierById[ADProofs](h.adProofsId).map(ps => Block(h, payload, Some(ps)))
    }

  override protected def validate(m: Payload): Try[Unit] =
    modifierValidation(m, typedModifierById[Header](m.headerId))

  private def putToHistory(payload: Payload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
