package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Shorts}
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.{DataBox, EncryBaseBox}
import encry.modifiers.state.box.proposition.ContractProposition
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import encrywm.common.{EncryContract, ScriptMeta}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

import scala.util.Try

case class DataDirective(script: EncryContract,
                         data: Array[Byte]) extends Directive {

  override type M = DataDirective

  override val typeId: DTypeId = DataDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(DataBox(ContractProposition(script), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), data))

  override val cost: Amount = 20 * script.meta.complexityScore

  override lazy val isValid: Boolean = data.length <= Constants.MaxDataLength && script.validMeta

  override def serializer: Serializer[M] = DataDirectiveSerializer
}

object DataDirective {

  val TypeId: DTypeId = 5.toByte

  // TODO: Serialize `script` as an object. Use `JsonSerializable`.
  implicit val jsonEncoder: Encoder[DataDirective] = (d: DataDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "script" -> Algos.encode(d.script.serializedScript).asJson,
    "complexityScore" -> d.script.meta.complexityScore.asJson,
    "scriptFingerprint" -> Algos.encode(d.script.meta.scriptFingerprint).asJson,
    "data" -> Algos.encode(d.data).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[DataDirective] = (c: HCursor) => {
    for {
      scriptStr <- c.downField("script").as[String]
      complexityScore <- c.downField("complexityScore").as[Int]
      scriptFingerprint <- c.downField("scriptFingerprint").as[String]
      data <- c.downField("data").as[String]
    } yield {
      DataDirective(
        Algos.decode(scriptStr).map(scriptDes =>
          EncryContract(
            scriptDes,
            // TODO: Throw exception here or let invalid fingerprint be compromised during validation?
            ScriptMeta(complexityScore, Algos.decode(scriptFingerprint).getOrElse(Array.emptyByteArray))
          )
        ).getOrElse(throw new Exception("Script decoding failed")),
        Algos.decode(data).getOrElse(throw new Exception("Data decoding failed"))
      )
    }
  }
}

object DataDirectiveSerializer extends Serializer[DataDirective] {

  override def toBytes(obj: DataDirective): Array[Byte] =
    Bytes.concat(
      Shorts.toByteArray(obj.script.serializedScript.length.toShort),
      obj.script.serializedScript,
      Ints.toByteArray(obj.script.meta.complexityScore),
      obj.script.meta.scriptFingerprint,
      Shorts.toByteArray(obj.data.length.toShort),
      obj.data
    )

  // TODO: Use constant for `ScriptFingerprint` length storing.
  override def parseBytes(bytes: Array[Byte]): Try[DataDirective] = Try {
    val scriptLen = Shorts.fromByteArray(bytes.take(2))
    val complexity = Ints.fromByteArray(bytes.slice(scriptLen + 2, scriptLen + 2 + 4))
    val fingerprint = bytes.slice(scriptLen + 2 + 4, scriptLen + 2 + 4 + 8)
    val contract = EncryContract(bytes.slice(2, scriptLen), ScriptMeta(complexity, fingerprint))
    val dataLen = Shorts.fromByteArray(bytes.slice(scriptLen + 2 + 4 + 8, scriptLen + 2 + 4 + 8 + 2))
    val data = bytes.slice(scriptLen + 2 + 4 + 8 + 2, scriptLen + 2 + 4 + 8 + 2 + dataLen)
    DataDirective(contract, data)
  }
}
