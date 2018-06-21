package scorex.core.transaction.proof

import encry.modifiers.BytesSerializable
import encry.view.state.Secret
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}

/**
  * The most general abstraction of fact a prover can provide a non-interactive proof
  * to open a box or to modify an account
  *
  * A proof is non-interactive and thus serializable
  */

trait Proof[P <: Proposition] extends BytesSerializable {
  def isValid(proposition: P, message: Array[Byte]): Boolean
}

trait ProofOfKnowledge[S <: Secret, P <: ProofOfKnowledgeProposition[S]] extends Proof[P]
