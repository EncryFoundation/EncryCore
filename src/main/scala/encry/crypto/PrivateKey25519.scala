package encry.crypto

import com.google.common.primitives.Bytes
import encry.modifiers.serialization.{BytesSerializable, Serializer}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.util.Try

trait SecretWrapper extends BytesSerializable

case class PrivateKey25519(privKeyBytes: PrivateKey, publicKeyBytes: PublicKey) extends SecretWrapper {

  override type M = PrivateKey25519

  lazy val publicImage: PublicKey25519 = PublicKey25519(publicKeyBytes)

  override def serializer: Serializer[M] = PrivateKey25519Serializer

  def sign(message: Array[Byte]): Signature25519 = Signature25519(Curve25519.sign(privKeyBytes, message))
}

object PrivateKey25519Serializer extends Serializer[PrivateKey25519] {

  override def toBytes(obj: PrivateKey25519): Array[Byte] = Bytes.concat(obj.privKeyBytes, obj.publicKeyBytes)

  override def parseBytes(bytes: Array[Byte]): Try[PrivateKey25519] = Try {
    PrivateKey25519(PrivateKey @@ bytes.slice(0, 32), PublicKey @@ bytes.slice(32, 64))
  }
}

object PrivateKey25519 {

  def sign(secret: PrivateKey25519, message: Array[Byte]): Signature25519 =
    Signature25519(Curve25519.sign(secret.privKeyBytes, message))

  def verify(message: Array[Byte], publicImage: PublicKey25519, proof: Signature25519): Boolean =
    Curve25519.verify(proof.signature, message, publicImage.pubKeyBytes)

  def generateKeys(randomSeed: Array[Byte]): (PrivateKey25519, PublicKey25519) = {
    val (secretKey: PrivateKey, pubKey) = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKey25519 = PrivateKey25519(secretKey, pubKey)
    secret -> secret.publicImage
  }
}
