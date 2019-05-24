package encry.view.history.processors.payload

import encry.consensus.History
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Payload
import scala.util.Try

trait  BaseBlockPayloadProcessor {

  protected def process(payload: Payload): History.ProgressInfo[PersistentModifier]

  protected def validate(payload: Payload): Try[Unit]
}