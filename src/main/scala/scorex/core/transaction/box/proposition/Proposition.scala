package scorex.core.transaction.box.proposition

import encry.modifiers.BytesSerializable
import scorex.core.transaction.state.Secret

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

