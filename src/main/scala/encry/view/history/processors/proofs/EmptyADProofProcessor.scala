package encry.view.history.processors.proofs

import encry.consensus.History.ProgressInfo
import encry.view.history.processors.ValidationError.FatalValidationError.IncorrectProcessingRegime
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.ADProofs
import cats.syntax.either._
import encry.view.history.processors.ValidationError

trait EmptyADProofProcessor extends BaseADProofProcessor {

  protected val adState: Boolean = false

  override protected def process(m: ADProofs): ProgressInfo[PersistentModifier] =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)

  override protected def validate(m: ADProofs): Either[ValidationError, PersistentModifier] =
    Either.left(IncorrectProcessingRegime("Regime that do not process ADProofs"))
}
