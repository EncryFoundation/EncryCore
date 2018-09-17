package encry.view.history.processors.payload

import encry.modifiers.EncryPersistentModifier
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.Payload

import scala.util.{Failure, Try}

trait EmptyBlockPayloadProcessor extends BaseBlockPayloadProcessor {

  override protected def validate(m: Payload): Try[Unit] =
    Failure(new Exception("Regime that do not process BlockTransactions"))

  override protected def process(m: Payload): ProgressInfo[EncryPersistentModifier] =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
}
