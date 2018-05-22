package encry.account

import encry.crypto.PublicKey25519
import encry.crypto.encoding.Base58Check
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.PublicKey

import scala.util.Try

// Represents the owner of the Public/Private key pair.
case class Account(address: Address) extends BytesSerializable {

  override type M = Account

  lazy val isValid: Boolean = Base58Check.decode(address).map(bytes =>
    if (bytes.length != PublicKey25519.Length) throw new Exception("Invalid address")
  ).isSuccess

  override def serializer: Serializer[M] = AccountSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case acc: Account if acc.address == address => true
    case _ => false
  }

  override def toString: String = address
}

object AccountSerializer extends Serializer[Account] {

  override def toBytes(obj: Account): Array[Byte] = Base58.decode(obj.address).get   // TODO: .get

  override def parseBytes(bytes: Array[Byte]): Try[Account] = Try(Account(Address @@ Base58.encode(bytes)))
}

object Account {

  val AddressLength: Int = 1 + PublicKey25519.Length + Base58Check.ChecksumLength

  def apply(publicKey: PublicKey): Account = Account(Address @@ Base58Check.encode(publicKey))

  def decodeAddress(address: Address): Array[Byte] = Base58.decode(address).get

  def validAddress(address: Address): Boolean = Account(address).isValid
}
