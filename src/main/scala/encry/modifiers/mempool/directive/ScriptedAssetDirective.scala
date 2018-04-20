package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.proposition.ContractProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import encrywm.common.{EncryContract, ScriptMeta}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class ScriptedAssetDirective(script: EncryContract,
                                  amount: Amount,
                                  override val idx: Int,
                                  tokenIdOpt: Option[ADKey] = None) extends Directive {

  override type M = ScriptedAssetDirective

  override val typeId: DTypeId = ScriptedAssetDirective.TypeId

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
    Seq(AssetBox(ContractProposition(script), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount))

  override val cost: Amount = 4 * script.meta.complexityScore

  override lazy val isValid: Boolean = amount > 0 && script.validMeta

  override def serializer: Serializer[M] = ScriptedAssetDirectiveSerializer

  lazy val isIntrinsic: Boolean = tokenIdOpt.isEmpty
}

object ScriptedAssetDirective {

  val TypeId: DTypeId = 4.toByte

  implicit val jsonEncoder: Encoder[ScriptedAssetDirective] = (d: ScriptedAssetDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "verboseType" -> "SCRIPT_LOCK".asJson,
    "script" -> Base58.encode(d.script.serializedScript).asJson,
    "complexityScore" -> d.script.meta.complexityScore.asJson,
    "scriptFingerprint" -> Base58.encode(d.script.meta.scriptFingerprint).asJson,
    "amount" -> d.amount.asJson,
    "tokenId" -> d.tokenIdOpt.map(id => Algos.encode(id)).getOrElse("null").asJson,
    "idx" -> d.idx.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[ScriptedAssetDirective] = (c: HCursor) => for {
    scriptStr <- c.downField("script").as[String]
    complexityScore <- c.downField("complexityScore").as[Int]
    scriptFingerprint <- c.downField("scriptFingerprint").as[String]
    amount <- c.downField("amount").as[Long]
    tokenIdOpt <- c.downField("tokenId").as[Option[String]]
    idx <- c.downField("idx").as[Int]
  } yield {
    ScriptedAssetDirective(
      Base58.decode(scriptStr).map(scriptDes =>
        EncryContract(
          scriptDes,
          ScriptMeta(complexityScore, Base58.decode(scriptFingerprint).getOrElse(Array.emptyByteArray))
        )
      ).getOrElse(throw new Exception("Incorrect script deserialize from json")),
      amount,
      idx,
      tokenIdOpt.flatMap(id => Algos.decode(id).map(ADKey @@ _).toOption)
    )
  }
}

object ScriptedAssetDirectiveSerializer extends Serializer[ScriptedAssetDirective] {

  override def toBytes(obj: ScriptedAssetDirective): Array[Byte] =
    Bytes.concat(
      Shorts.toByteArray(obj.script.serializedScript.length.toShort),
      obj.script.serializedScript,
      Ints.toByteArray(obj.script.meta.complexityScore),
      obj.script.meta.scriptFingerprint,
      Longs.toByteArray(obj.amount),
      Ints.toByteArray(obj.idx),
      obj.tokenIdOpt.getOrElse(Array.empty)
    )

  override def parseBytes(bytes: Array[Byte]): Try[ScriptedAssetDirective] = Try {
    val scriptLen = Shorts.fromByteArray(bytes.take(2))
    val complexity = Ints.fromByteArray(bytes.slice(scriptLen + 2, scriptLen + 2 + 4))
    val fingerprint = bytes.slice(scriptLen + 2 + 4, scriptLen + 2 + 4 + 8)
    val contract = EncryContract(bytes.slice(2, scriptLen), ScriptMeta(complexity, fingerprint))
    val amount = Longs.fromByteArray(bytes.slice(scriptLen + 2 + 4 + 8, scriptLen + 2 + 4 + 8 + 8))
    val idx = Ints.fromByteArray(bytes.slice(scriptLen + 2 + 4 + 8 + 8, scriptLen + 2 + 4 + 8 + 8 + 4))
    val tokenIdOpt = if ((bytes.length - (scriptLen + 2 + 4 + 8 + 8 + 4)) == Constants.ModifierIdSize) {
      Some(ADKey @@ bytes.takeRight(Constants.ModifierIdSize))
    } else None
    ScriptedAssetDirective(contract, amount, idx, tokenIdOpt)
  }
}
