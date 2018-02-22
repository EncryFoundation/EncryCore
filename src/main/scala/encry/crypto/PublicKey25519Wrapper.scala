package encry.crypto

import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class PublicKey25519(pubKeyBytes: PublicKey) extends PublicKeyWrapper {

  require(pubKeyBytes.length == Curve25519.KeyLength,
    s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} given")

  import PublicKey25519._

  override type M = PublicKey25519

  private def bytesWithVersion: Array[Byte] = AddressVersion +: pubKeyBytes

  lazy val address: String = Base58.encode(bytesWithVersion ++ calcCheckSum(bytesWithVersion))

  def verify(message: Array[Byte], signature: Signature): Boolean = Curve25519.verify(signature, message, pubKeyBytes)

  override def serializer: Serializer[M] = PublicKey25519Serializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: PublicKey25519 => p.pubKeyBytes sameElements pubKeyBytes
    case _ => false
  }

  override def hashCode(): Int = (BigInt(pubKeyBytes) % Int.MaxValue).toInt
}

object PublicKey25519Serializer extends Serializer[PublicKey25519] {

  override def toBytes(obj: PublicKey25519): Array[Byte] = obj.pubKeyBytes

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519] = Try(PublicKey25519(PublicKey @@ bytes))
}

object PublicKey25519 {

  private val ChecksumLength = 4

  val AddressVersion: Byte = 1
  val PubKeyLength: Int = 32
  val AddressLength: Int = 1 + PubKeyLength + ChecksumLength

  def calcCheckSum(bytes: Array[Byte]): Array[Byte] = Blake2b256.hash(bytes).take(ChecksumLength)

  def validPubKey(address: String): Try[PublicKey25519] =
    Base58.decode(address).flatMap { addressBytes =>
      if (addressBytes.length != AddressLength)
        Failure(new Exception("Wrong address length"))
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)

        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))

        if (checkSum.sameElements(checkSumGenerated))
          Success(PublicKey25519(PublicKey @@ addressBytes.dropRight(ChecksumLength).tail))
        else Failure(new Exception("Wrong checksum"))
      }
    }
}
