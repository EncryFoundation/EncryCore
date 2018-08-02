package encry.crypto

import encry.modifiers.mempool.Pay2PubKeyAddress
import encry.modifiers.serialization.{BytesSerializable, Serializer}
import encry.settings.Algos
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scala.util.Try

case class PublicKey25519(pubKeyBytes: PublicKey) extends BytesSerializable {

  require(pubKeyBytes.length == Curve25519.KeyLength,
    s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} given")

  override type M = PublicKey25519

  lazy val address: Pay2PubKeyAddress = Pay2PubKeyAddress(pubKeyBytes)

  override def serializer: Serializer[M] = PublicKey25519Serializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: PublicKey25519 => ByteArrayWrapper(p.pubKeyBytes) == ByteArrayWrapper(pubKeyBytes)
    case _ => false
  }

  override def hashCode: Int = (BigInt(pubKeyBytes) % Int.MaxValue).toInt

  override def toString: String = Algos.encode(pubKeyBytes)
}

object PublicKey25519Serializer extends Serializer[PublicKey25519] {

  override def toBytes(obj: PublicKey25519): Array[Byte] = obj.pubKeyBytes

  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519] = Try(PublicKey25519(PublicKey @@ bytes))
}

object PublicKey25519 { val Length: Int = 32 }
