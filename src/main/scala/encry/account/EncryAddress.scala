package encry.account

/** P2PKH - 160-bit hash of the publicKey
  * P2PK  - serialized (compressed) public key
  * P2sH  - 160 bit of the script */
sealed trait EncryAddress {
  val typePrefix: Byte
}

case class Pay2PublicKeyAddress()
