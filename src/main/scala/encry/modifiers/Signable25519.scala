package encry.modifiers

import encry.crypto.{PublicKey25519, Signature25519}

trait Signable25519 {

  val accountPubKey: PublicKey25519

  val dataToSign: Array[Byte]

  val signature: Signature25519

  def validSignature: Boolean = signature.isValid(accountPubKey, dataToSign)
}
