package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import encry.view.history.processors.ValidationError
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Payload
import cats.syntax.either._
import encry.view.history.processors.ValidationError.FatalValidationError.IncorrectProcessingRegime

trait EmptyBlockPayloadProcessor extends BaseBlockPayloadProcessor {

  override protected def validate(m: Payload): Either[ValidationError, PersistentModifier] =
    IncorrectProcessingRegime("Regime that do not process BlockTransactions").asLeft[PersistentModifier]

  override protected def process(m: Payload): ProgressInfo =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
}