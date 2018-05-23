package encry.view.history.processors.payload

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

trait EmptyBlockPayloadProcessor extends BaseBlockPayloadProcessor {

  override protected def validate(m: EncryBlockPayload): Try[Unit] =
    Failure(new Exception("Regime that do not process BlockTransactions"))

  override protected def process(m: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
}
