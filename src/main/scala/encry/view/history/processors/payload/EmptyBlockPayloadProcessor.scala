package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Payload
import scala.util.{Failure, Try}

trait EmptyBlockPayloadProcessor extends BaseBlockPayloadProcessor {

  override protected def validate(m: Payload): Try[Unit] =
    Failure(new Exception("Regime that do not process BlockTransactions"))

  override protected def process(m: Payload): ProgressInfo[PersistentModifier] =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
}