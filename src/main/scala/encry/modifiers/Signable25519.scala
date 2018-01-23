package encry.modifiers

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

trait Signable25519 {

  val proposition: PublicKey25519Proposition

  val dataToSign: Array[Byte]

  val signature: Signature25519

  lazy val validSignature: Boolean = signature.isValid(proposition, dataToSign)
}
