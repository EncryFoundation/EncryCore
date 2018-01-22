package encry.view.history.processors.proofs

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

trait EmptyADProofProcessor extends BaseADProofProcessor {

  protected val adState: Boolean = false

  override protected def process(m: ADProofs): ProgressInfo[EncryPersistentModifier] =
    ProgressInfo(None, Seq(), None, Seq())

  override protected def validate(m: ADProofs): Try[Unit] = Failure(new Error("Regime that do not process ADProofs"))
}
