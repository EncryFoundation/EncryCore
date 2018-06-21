package scorex.core.transaction.box.proposition

import encry.modifiers.BytesSerializable
import encry.view.state.Secret

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition

