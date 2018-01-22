package encry.view.history.processors.proofs

/**
  * ADProof processor for regime that validate transactions via ADProofs.
  */
trait ADStateProofProcessor extends FullProofProcessor {

  protected val adState: Boolean = true
}
