package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import encry.view.history.processors.{BlockProcessor, ValidationError}
import encry.view.history.storage.HistoryStorage
import encry.settings.EncryAppSettings
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ADProofs, Block, Header, Payload}

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val settings: EncryAppSettings

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(payload: Payload): ProgressInfo[PersistentModifier] =
    getBlockByPayload(payload)
      .flatMap(block =>
        if (block.header.height - bestBlockHeight >= 2 + settings.network.maxInvObjects) None
        else Some(processBlock(block, payload))
      ).getOrElse(putToHistory(payload))

  private def getBlockByPayload(payload: Payload): Option[Block] = headersCache.get(ByteArrayWrapper(payload.headerId))
    .orElse(typedModifierById[Header](payload.headerId)).flatMap { h =>
      if (!adState) Some(Block(h, payload, None))
      else typedModifierById[ADProofs](h.adProofsId).map(ps => Block(h, payload, Some(ps)))
    }

  override protected def validate(m: Payload): Either[ValidationError, PersistentModifier] =
    modifierValidation(m, headersCache.get(ByteArrayWrapper(m.headerId)).orElse(typedModifierById[Header](m.headerId)))

  private def putToHistory(payload: Payload): ProgressInfo[PersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}