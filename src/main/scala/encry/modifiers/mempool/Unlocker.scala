package encry.modifiers.mempool

import com.google.common.primitives.Shorts
import encry.modifiers.state.box.proof.{Proof, ProofSerializer}
import encry.settings.{Algos, Constants}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.{BytesSerializable, SerializationException, Serializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

/** Holds the boxId/proof pair. */
case class Unlocker(boxId: ADKey,
                    proofs: List[Proof],
                    namedProofs: List[(String, Proof)] = List.empty) extends BytesSerializable {

  override type M = Unlocker

  override def serializer: Serializer[M] = UnlockerSerializer

  lazy val bytesWithoutProof: Array[Byte] = UnlockerSerializer.toBytesWithoutProof(this)
}

object Unlocker {

  implicit val jsonEncoder: Encoder[Unlocker] = (u: Unlocker) => Map(
    "boxId" -> Algos.encode(u.boxId).asJson,
    "proofs" -> u.proofs.map(_.asJson),
    "namedProofs" -> u.namedProofs.map(_.asJson)
  ).asJson

  implicit val jsonDecoder: Decoder[Unlocker] = (c: HCursor) => {
    for {
      boxId <- c.downField("boxId").as[String]
      proofs <- c.downField("proof").as[List[Proof]]
      namedProofs <- c.downField("namedProofs").as[List[(String, Proof)]]
    } yield {
      Unlocker(ADKey @@ Algos.decode(boxId).get, proofs, namedProofs)
    }
  }
}

object UnlockerSerializer extends Serializer[Unlocker] {

  def toBytesWithoutProof(obj: Unlocker): Array[Byte] = obj.boxId

  override def toBytes(obj: Unlocker): Array[Byte] = if (obj.proofs.isEmpty && obj.namedProofs.isEmpty) obj.boxId else {
    val proofsBytes: Array[Byte] = obj.proofs.foldLeft(Array.empty[Byte]) { case (acc, proof) =>
      val proofBytes: Array[Byte] = ProofSerializer.toBytes(proof)
      acc ++ Shorts.toByteArray(proofBytes.length.toShort) ++ proofBytes
    }
    val namedProofsBytes: Array[Byte] = obj.namedProofs.foldLeft(Array.empty[Byte]) { case (acc, (name, proof)) =>
      val proofBytes: Array[Byte] = ProofSerializer.toBytes(proof)
      val nameBytes: Array[Byte] = name.getBytes(Algos.charset)
      acc ++ Array(nameBytes.length.toByte) ++ nameBytes ++ Shorts.toByteArray(proofBytes.length.toShort) ++ proofBytes
    }
    obj.boxId ++ Array(obj.proofs.size.toByte) ++ proofsBytes ++ Array(obj.namedProofs.size.toByte) ++ namedProofsBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[Unlocker] = Try {
    if (bytes.lengthCompare(Constants.ModifierIdSize) == 0) Unlocker(ADKey @@ bytes, List.empty)
    else {
      val boxId: ADKey = ADKey @@ bytes.take(Constants.ModifierIdSize)
      val proofsQty: Int = bytes.drop(Constants.ModifierIdSize).head
      val (proofs, leftBytes) = (0 to proofsQty).foldLeft(List.empty[Proof], bytes.drop(Constants.ModifierIdSize + 1)) { case ((acc, bytesAcc), _) =>
        val proofLen: Int = Shorts.fromByteArray(bytesAcc.take(2))
        val proof: Proof = ProofSerializer.parseBytes(bytesAcc.slice(2, proofLen + 2)).getOrElse(throw SerializationException)
        (acc :+ proof) -> bytesAcc.drop(proofLen + 2)
      }
      val namedProofsQty: Int = leftBytes.head
      val (namedProofs, _) = (0 to namedProofsQty).foldLeft(List.empty[(String, Proof)], leftBytes.tail) { case ((acc, bytesAcc), _) =>
        val nameLen: Int = bytesAcc.head
        val name: String = new String(bytesAcc.slice(1, 1 + nameLen), Algos.charset)
        val proofLen: Int = Shorts.fromByteArray(bytesAcc.slice(1 + nameLen, 1 + nameLen + 2))
        val proof: Proof = ProofSerializer.parseBytes(bytesAcc.slice(1 + nameLen + 2, 1 + nameLen + 2 + proofLen)).getOrElse(throw SerializationException)
        (acc :+ (name -> proof)) -> bytesAcc.drop(1 + nameLen + 2 + proofLen)
      }
      Unlocker(boxId, proofs, namedProofs)
    }
  }
}
