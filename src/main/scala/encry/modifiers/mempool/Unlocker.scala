package encry.modifiers.mempool

import encry.modifiers.state.box.EncryBox
import encry.modifiers.state.box.proof.{Proof, ProofSerializer}
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.{BytesSerializable, JsonSerializable, Serializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

// Holds the boxId/proof pair.
case class Unlocker(boxId: ADKey, proofOpt: Option[Proof]) extends BytesSerializable with JsonSerializable {

  override type M = Unlocker

  override def serializer: Serializer[M] = UnlockerSerializer

  override def json: Json = Map(
    "boxId" -> Algos.encode(boxId).asJson,
    "proof" -> proofOpt.map(_.json).getOrElse("None".asJson)
  ).asJson
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
