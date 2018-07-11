package encry.view.history.processors.payload

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.consensus.History.ProgressInfo
import encry.utils.Logging

import scala.util.{Failure, Try}

trait EmptyBlockPayloadProcessor extends BaseBlockPayloadProcessor with Logging {

  override protected def validate(m: EncryBlockPayload): Try[Unit] =
    Failure(new Exception("Regime that do not process BlockTransactions"))

  override protected def process(m: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    logger.trace("case - EmptyBlockPayloadProcessor")
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
