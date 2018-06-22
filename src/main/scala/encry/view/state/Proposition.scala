package encry.view.state

import encry.modifiers.BytesSerializable

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

