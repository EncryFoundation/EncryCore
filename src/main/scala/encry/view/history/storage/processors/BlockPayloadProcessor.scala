package encry.view.history.storage.processors

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.core.consensus.History

import scala.util.Try

trait BlockPayloadProcessor extends EncryBaseProcessor[EncryBlockPayload] {

  protected def process(payload: EncryBlockPayload): History.ProgressInfo[EncryPersistentModifier]

  protected def validate(payload: EncryBlockPayload): Try[Unit]
}
