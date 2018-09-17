package encry.view.history.processors.payload

import encry.consensus.History
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.Payload

import scala.util.Try

trait  BaseBlockPayloadProcessor {

  protected def process(payload: Payload): History.ProgressInfo[EncryPersistentModifier]

  protected def validate(payload: Payload): Try[Unit]
}
