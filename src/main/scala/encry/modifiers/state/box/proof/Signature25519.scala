package encry.modifiers.state.box.proof

import encry.crypto.PublicKey25519
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encry.settings.Algos
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
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

  override val esType: Types.Product = Types.Signature25519

  override def asVal: PValue = PValue(Types.EncryProof)(convert)

  override def convert: PObject = {
    val fields: Map[String, PValue] = Map(
      "sigBytes" -> PValue(Types.PCollection.ofByte)(signature),
      "typeId" -> PValue(Types.PInt)(typeId.toInt)
    )
    PObject(fields, esType)
  }
}

object Signature25519 {

  val TypeId: ProofTypeId = 1.toByte

  lazy val SignatureSize: Int = Curve25519.SignatureLength

  implicit val jsonEncoder: Encoder[Signature25519] = (p: Signature25519) => Map(
    "typeId" -> p.typeId.asJson,
    "signature" -> Algos.encode(p.signature).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[Signature25519] =
    (c: HCursor) => {
      for {
        sig <- c.downField("signature").as[String]
      } yield {
        Signature25519(Signature @@ Algos.decode(sig).get)
      }
    }
}

object Signature25519Serializer extends Serializer[Signature25519] {

  override def toBytes(obj: Signature25519): Array[Byte] = obj.signature

  override def parseBytes(bytes: Array[Byte]): Try[Signature25519] = Try(Signature25519(Signature @@ bytes))
}
