package encry.modifiers.mempool

import com.google.common.primitives.Shorts
import encry.settings.{Algos, Constants}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.{BytesSerializable, SerializationException, Serializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

case class Unlocker(boxId: ADKey,
                    proofs: List[Proof]) extends BytesSerializable {

  override type M = Unlocker

  override def serializer: Serializer[M] = UnlockerSerializer

  lazy val bytesWithoutProof: Array[Byte] = UnlockerSerializer.toBytesWithoutProof(this)
}

object Unlocker {

  implicit val jsonEncoder: Encoder[Unlocker] = (u: Unlocker) => Map(
    "boxId" -> Algos.encode(u.boxId).asJson,
    "proofs" -> u.proofs.map(_.asJson)
  ).asJson

  implicit val jsonDecoder: Decoder[Unlocker] = (c: HCursor) => {
    for {
      boxId <- c.downField("boxId").as[String]
      proofs <- c.downField("proof").as[List[Proof]]
    } yield {
      Unlocker(ADKey @@ Algos.decode(boxId).get, proofs)
    }
  }
}

object UnlockerSerializer extends Serializer[Unlocker] {

  def toBytesWithoutProof(obj: Unlocker): Array[Byte] = obj.boxId

  override def toBytes(obj: Unlocker): Array[Byte] = if (obj.proofs.isEmpty) obj.boxId else {
    val proofsBytes: Array[Byte] = obj.proofs.foldLeft(Array.empty[Byte]) { case (acc, proof) =>
      val proofBytes: Array[Byte] = ProofSerializer.toBytes(proof)
      acc ++ Shorts.toByteArray(proofBytes.length.toShort) ++ proofBytes
    }
    obj.boxId ++ Array(obj.proofs.size.toByte) ++ proofsBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[Unlocker] = Try {
    if (bytes.lengthCompare(Constants.ModifierIdSize) == 0) Unlocker(ADKey @@ bytes, List.empty)
    else {
      val boxId: ADKey = ADKey @@ bytes.take(Constants.ModifierIdSize)
      val proofsQty: Int = bytes.drop(Constants.ModifierIdSize).head
      val (proofs, _) = (0 to proofsQty).foldLeft(List.empty[Proof], bytes.drop(Constants.ModifierIdSize + 1)) { case ((acc, bytesAcc), _) =>
        val proofLen: Int = Shorts.fromByteArray(bytesAcc.take(2))
        val proof: Proof = ProofSerializer.parseBytes(bytesAcc.slice(2, proofLen + 2)).getOrElse(throw SerializationException)
        (acc :+ proof) -> bytesAcc.drop(proofLen + 2)
      }
      Unlocker(boxId, proofs)
    }
  }
}
