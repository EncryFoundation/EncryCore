package encry.modifiers.mempool

import com.google.common.primitives.Shorts
import encry.settings.{Algos, Constants}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.{BytesSerializable, SerializationException, Serializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

case class Input(boxId: ADKey, proofs: List[Proof]) extends BytesSerializable {

  override type M = Input

  override def serializer: Serializer[M] = InputSerializer

  lazy val bytesWithoutProof: Array[Byte] = InputSerializer.toBytesWithoutProof(this)

  def isUnsigned: Boolean = proofs.isEmpty
}

object Input {

  def unsigned(boxId: ADKey): Input = Input(boxId, List.empty)

  implicit val jsonEncoder: Encoder[Input] = (u: Input) => Map(
    "boxId" -> Algos.encode(u.boxId).asJson,
    "proofs" -> u.proofs.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[Input] = (c: HCursor) => {
    for {
      boxId <- c.downField("boxId").as[String]
      proofs <- c.downField("proofs").as[List[Proof]]
    } yield {
      Input(ADKey @@ Algos.decode(boxId).get, proofs)
    }
  }
}

object InputSerializer extends Serializer[Input] {

  def toBytesWithoutProof(obj: Input): Array[Byte] = obj.boxId

  override def toBytes(obj: Input): Array[Byte] = if (obj.isUnsigned) obj.boxId else {
    val proofsBytes: Array[Byte] = obj.proofs.foldLeft(Array.empty[Byte]) { case (acc, proof) =>
      val proofBytes: Array[Byte] = ProofSerializer.toBytes(proof)
      acc ++ Shorts.toByteArray(proofBytes.length.toShort) ++ proofBytes
    }
    obj.boxId ++ Array(obj.proofs.size.toByte) ++ proofsBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[Input] = Try {
    if (bytes.lengthCompare(Constants.ModifierIdSize) == 0) Input(ADKey @@ bytes, List.empty)
    else {
      val boxId: ADKey = ADKey @@ bytes.take(Constants.ModifierIdSize)
      val proofsQty: Int = bytes.drop(Constants.ModifierIdSize).head
      val (proofs, _) = (0 to proofsQty).foldLeft(List.empty[Proof], bytes.drop(Constants.ModifierIdSize + 1)) { case ((acc, bytesAcc), _) =>
        val proofLen: Int = Shorts.fromByteArray(bytesAcc.take(2))
        val proof: Proof = ProofSerializer.parseBytes(bytesAcc.slice(2, proofLen + 2)).getOrElse(throw SerializationException)
        (acc :+ proof) -> bytesAcc.drop(proofLen + 2)
      }
      Input(boxId, proofs)
    }
  }
}
