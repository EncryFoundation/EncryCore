package encry.view.state

import encry.modifiers.BytesSerializable


trait ProofOfKnowledge[S <: Secret, P <: ProofOfKnowledgeProposition[S]] extends BytesSerializable {
  def isValid(proposition: P, message: Array[Byte]): Boolean
}