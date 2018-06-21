package encry.crypto

import encry.modifiers.{BytesSerializable, Serializer}
import encry.settings.Algos
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.signatures.{Curve25519, Signature}

import scala.util.Try

case class Signature25519(signature: Signature) extends BytesSerializable {

  override type M = Signature25519

  def isValid(pubKey: PublicKey25519, message: Array[Byte]): Boolean =
    signature.isEmpty || signature.length == Curve25519.SignatureLength &&
      Curve25519.verify(signature, message, pubKey.pubKeyBytes)

  override def serializer: Serializer[Signature25519] = Signature25519Serializer
}

object Signature25519 {

  lazy val SignatureSize: Int = Curve25519.SignatureLength

  implicit val jsonEncoder: Encoder[Signature25519] = (p: Signature25519) => Map(
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