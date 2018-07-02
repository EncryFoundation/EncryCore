package encry.view.state

import encry.modifiers.serialization.Serializer
import encry.settings.Algos
import scorex.crypto.signatures.{Curve25519, Signature}

import scala.util.Try

case class Signature25519(signature: Signature) extends ProofOfKnowledge[PrivateKey25519, PublicKey25519Proposition] {
  require(signature.isEmpty || signature.length == Curve25519.SignatureLength,
    s"${signature.length} != ${Curve25519.SignatureLength}")

  override def isValid(proposition: PublicKey25519Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.pubKeyBytes)

  override type M = Signature25519

  override def serializer: Serializer[Signature25519] = Signature25519Serializer

  override def toString: String = s"Signature25519(${Algos.encode(signature)})"
}

object Signature25519Serializer extends Serializer[Signature25519] {
  override def toBytes(obj: Signature25519): Array[Byte] = obj.signature

  override def parseBytes(bytes: Array[Byte]): Try[Signature25519] = Try(Signature25519(Signature @@ bytes))
}

object Signature25519 {
  lazy val SignatureSize: Int = Curve25519.SignatureLength
}