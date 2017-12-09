package encry.modifiers.mempool.box.unlockers

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Proof

import scala.util.Try

class EncryPaymentBoxProof(message: Array[Byte]) extends Proof[PublicKey25519Proposition] {

  // TODO: Always returns `true`!
  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean = true

  override def serializer: Serializer[EncryPaymentBoxProof] = EncryPaymentBoxProofSerializer
}

object EncryPaymentBoxProofSerializer extends Serializer[EncryPaymentBoxProof] {

  override def toBytes(obj: EncryPaymentBoxProof): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryPaymentBoxProof] = ???
}
