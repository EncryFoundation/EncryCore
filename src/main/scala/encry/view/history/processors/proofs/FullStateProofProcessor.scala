package encry.view.history.processors.proofs

/**
  * ADProof processor for regime that validate transactions via full state
  * and collect ADProofs to send them to light nodes
  */
trait FullStateProofProcessor extends FullProofProcessor {

  protected val adState: Boolean = false
}
