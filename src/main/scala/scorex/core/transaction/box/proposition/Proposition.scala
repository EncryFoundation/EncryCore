package scorex.core.transaction.box.proposition

import scorex.core.serialization.BytesSerializable

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S] extends Proposition

