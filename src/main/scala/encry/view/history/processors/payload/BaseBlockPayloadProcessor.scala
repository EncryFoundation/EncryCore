package encry.view.history.processors.payload

import encry.consensus.History
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.EncryBlockPayload

import scala.util.Try

trait  BaseBlockPayloadProcessor {

  protected def process(payload: EncryBlockPayload): History.ProgressInfo[EncryPersistentModifier]

  protected def validate(payload: EncryBlockPayload): Try[Unit]
}
