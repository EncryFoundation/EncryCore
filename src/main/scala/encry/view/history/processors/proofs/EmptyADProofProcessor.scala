package encry.view.history.processors.proofs

import encry.consensus.History.ProgressInfo
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.ADProofs
import scala.util.{Failure, Try}

trait EmptyADProofProcessor extends BaseADProofProcessor {

  protected val adState: Boolean = false

  override protected def process(m: ADProofs): ProgressInfo[PersistentModifier] =
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)

  override protected def validate(m: ADProofs): Try[Unit] = Failure(new Exception("Regime that do not process ADProofs"))
}
