package encry.modifiers.mempool

import encry.modifiers.state.box.EncryBox
import encry.modifiers.state.box.proof.{Proof, ProofSerializer}
import encry.settings.Algos
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types._
import encrywm.lib.predef.env.ESEnvConvertable
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

/** Holds the boxId/proof pair. */
case class Unlocker(boxId: ADKey, proofOpt: Option[Proof]) extends BytesSerializable with ESEnvConvertable {

  override type M = Unlocker

  override def serializer: Serializer[M] = UnlockerSerializer

  lazy val bytesWithoutProof: Array[Byte] = UnlockerSerializer.toBytesWithoutProof(this)

  override def asVal: ESValue = ESValue(Types.ESUnlocker.ident.toLowerCase, Types.ESUnlocker)(convert)

  override val esType: Types.ESProduct = ESUnlocker

  override def convert: ESObject = {
    val fields = Map(
      "boxId" -> ESValue("boxId", ESByteVector)(boxId),
      "proofOpt" -> ESValue("proofOpt", ESOption(ESProof))(proofOpt.map(_.convert))
    )
    ESObject(Types.ESUnlocker.ident, fields, esType)
  }
}

object Unlocker {

  implicit val jsonEncoder: Encoder[Unlocker] = (u: Unlocker) => Map(
    "boxId" -> Algos.encode(u.boxId).asJson,
    "proof" -> u.proofOpt.map(_.asJson).getOrElse("None".asJson)
  ).asJson

  implicit val jsonDecoder: Decoder[Unlocker] = (c: HCursor) => {
    for {
      boxId <- c.downField("boxId").as[String]
      proof <- c.downField("proof").as[Option[Proof]]
    } yield {
      Unlocker(ADKey @@ Algos.decode(boxId).get, proof)
    }
  }
}

object UnlockerSerializer extends Serializer[Unlocker] {

  def toBytesWithoutProof(obj: Unlocker): Array[Byte] = obj.boxId

  override def toBytes(obj: Unlocker): Array[Byte] =
    obj.boxId ++ obj.proofOpt.map(ProofSerializer.toBytes).getOrElse(Array.empty)

  override def parseBytes(bytes: Array[Byte]): Try[Unlocker] = Try {
    val boxId = ADKey @@ bytes.take(EncryBox.BoxIdSize)
    val proof = if (bytes.length == EncryBox.BoxIdSize) None
      else Some(ProofSerializer.parseBytes(bytes.drop(EncryBox.BoxIdSize)).get)
    Unlocker(boxId, proof)
  }
}
