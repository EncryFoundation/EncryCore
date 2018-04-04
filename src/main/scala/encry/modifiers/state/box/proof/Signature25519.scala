package encry.modifiers.state.box.proof

import encry.crypto.PublicKey25519
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encry.settings.Algos
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.{ESByteVector, ESInt, ESProof}
import io.circe.Encoder
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

  override val esType: Types.ESProduct = Types.Signature25519

  override def asVal: ESValue = ESValue("proof", ESProof)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "sigBytes" -> ESValue("sigBytes", ESByteVector)(signature),
      "typeId" -> ESValue("typeId", ESInt)(typeId)
    )
    ESObject(Types.Signature25519.ident, fields, esType)
  }
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
