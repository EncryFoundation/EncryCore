package encry.modifiers.mempool

import encry.modifiers.state.box.EncryBox
import encry.modifiers.state.box.proof.{Proof, ProofSerializer}
import encry.settings.Algos
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

// Holds the boxId/proof pair.
case class Unlocker(boxId: ADKey, proofOpt: Option[Proof]) extends BytesSerializable {

  override type M = Unlocker

  override def serializer: Serializer[M] = UnlockerSerializer
}

object Unlocker {

  implicit val jsonEncoder: Encoder[Unlocker] = (u: Unlocker) => Map(
    "boxId" -> Algos.encode(u.boxId).asJson,
    "proof" -> u.proofOpt.map(_.asJson).getOrElse("None".asJson)
  ).asJson

  implicit val jsonDecoder: Decoder[Unlocker] = {
    (c: HCursor) => {
      for {
        foo <- c.downField("boxId").as[String]
        bar <- c.downField("proof").as[Option[Proof]]
      } yield {
        Unlocker(ADKey @@ Algos.decode(foo).get, bar)
      }
    }
  }
}

object UnlockerSerializer extends Serializer[Unlocker] {

  override def toBytes(obj: Unlocker): Array[Byte] =
    obj.boxId ++ obj.proofOpt.map(ProofSerializer.toBytes).getOrElse(Array.empty)

  override def parseBytes(bytes: Array[Byte]): Try[Unlocker] = Try {
    val boxId = ADKey @@ bytes.take(EncryBox.BoxIdSize)
    val proof = if (bytes.length == EncryBox.BoxIdSize) None
      else Some(ProofSerializer.parseBytes(bytes.drop(EncryBox.BoxIdSize)).get)
    Unlocker(boxId, proof)
  }
}
