package encry.view.history.processors.proofs

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

trait BaseADProofProcessor {

  /**
    * Root hash only is kept in state
    */
  protected val adState: Boolean

  /**
    * @param m - modifier to process
    * @return ProgressInfo - info required for State to be consistent with History
    */
  protected def process(m: ADProofs): ProgressInfo[EncryPersistentModifier]

  /**
    * @param m - ADProof to validate
    * @return Success() if ADProof is valid from History point of view, Failure(error) otherwise
    */
  protected def validate(m: ADProofs): Try[Unit]
}
