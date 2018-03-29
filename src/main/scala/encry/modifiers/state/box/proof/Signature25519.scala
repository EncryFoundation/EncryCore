package encry.modifiers.state.box.proof

import encry.crypto.PublicKey25519
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encry.settings.Algos
import io.circe.{Encoder, Json}
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.crypto.signatures.{Curve25519, Signature}

import scala.util.Try

case class Signature25519(signature: Signature) extends Proof {

  override type M = Signature25519

  override val typeId: ProofTypeId = Signature25519.TypeId

  def isValid(pubKey: PublicKey25519, message: Array[Byte]): Boolean =
    signature.isEmpty || signature.length == Curve25519.SignatureLength &&
      Curve25519.verify(signature, message, pubKey.pubKeyBytes)

  override def serializer: Serializer[Signature25519] = Signature25519Serializer
}

object Signature25519 {

  val TypeId: ProofTypeId = 1.toByte

  lazy val SignatureSize: Int = Curve25519.SignatureLength

  implicit val jsonEncoder: Encoder[Signature25519] = (p: Signature25519) => Map(
    "signature" -> Algos.encode(p.signature).asJson
  ).asJson
}

object Signature25519Serializer extends Serializer[Signature25519] {

  override def toBytes(obj: Signature25519): Array[Byte] = obj.signature

  override def parseBytes(bytes: Array[Byte]): Try[Signature25519] = Try(Signature25519(Signature @@ bytes))
}
