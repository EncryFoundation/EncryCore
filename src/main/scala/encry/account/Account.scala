package encry.account

import encry.crypto.PublicKey25519
import encry.crypto.encoding.Base58Check
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.signatures.PublicKey

import scala.util.{Failure, Success, Try}

// Represents the owner of the Public/Private key pair.
case class Account(address: Address) extends BytesSerializable {

  override type M = Account

  lazy val isValid: Boolean = Base58Check.decode(address).flatMap(bytes =>
    if (bytes.length == PublicKey25519.Length) Success(bytes) else Failure(new Exception("Length mismatch"))).isSuccess

  override def serializer: Serializer[M] = AccountSerializer
}

object AccountSerializer extends Serializer[Account] {

  override def toBytes(obj: Account): Array[Byte] = Base58Check.decode(obj.address).get   // TODO: .get

  override def parseBytes(bytes: Array[Byte]): Try[Account] = Try(Account(Address @@ Base58Check.encode(bytes)))
}

object Account {

  def apply(publicKey: PublicKey): Account = Account(Address @@ Base58Check.encode(publicKey))
}
