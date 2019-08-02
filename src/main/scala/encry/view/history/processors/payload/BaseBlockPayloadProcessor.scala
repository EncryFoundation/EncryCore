package encry.view.history.processors.payload

import encry.consensus.History
import encry.view.history.processors.ValidationError
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Payload

trait  BaseBlockPayloadProcessor {

  protected def process(payload: Payload): History.ProgressInfo

  protected def validate(payload: Payload): Either[ValidationError, PersistentModifier]
}