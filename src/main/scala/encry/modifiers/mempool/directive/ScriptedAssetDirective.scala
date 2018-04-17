package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.proposition.ContractProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.utils.Utils
import encrywm.common.{ESContract, ScriptMeta}
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class ScriptedAssetDirective(script: ESContract,
                                  amount: Amount,
                                  override val idx: Int) extends Directive {

  override type M = ScriptedAssetDirective

  override val typeId: DTypeId = ScriptedAssetDirective.TypeId

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
    Seq(AssetBox(ContractProposition(script), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount))

  override val cost: Amount = 4 * script.meta.complexityScore

  override lazy val isValid: Boolean = amount > 0 && script.validMeta

  override def serializer: Serializer[M] = ScriptedAssetDirectiveSerializer
}

object ScriptedAssetDirective {

  val TypeId: DTypeId = 4.toByte

  implicit val jsonEncoder: Encoder[ScriptedAssetDirective] = (d: ScriptedAssetDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "verboseType" -> "SCRIPT_LOCK".asJson,
    "script" -> Base58.encode(d.script.serializedScript).asJson,
    "amount" -> d.amount.asJson,
    "idx" -> d.idx.asJson
  ).asJson
}

object ScriptedAssetDirectiveSerializer extends Serializer[ScriptedAssetDirective] {

  override def toBytes(obj: ScriptedAssetDirective): Array[Byte] =
    Bytes.concat(
      Shorts.toByteArray(obj.script.serializedScript.length.toShort),
      obj.script.serializedScript,
      Ints.toByteArray(obj.script.meta.complexityScore),
      obj.script.meta.scriptFingerprint,
      Longs.toByteArray(obj.amount),
      Ints.toByteArray(obj.idx)
    )

  override def parseBytes(bytes: Array[Byte]): Try[ScriptedAssetDirective] = Try {
    val scriptLen = Shorts.fromByteArray(bytes.take(2))
    val complexity = Ints.fromByteArray(bytes.slice(scriptLen + 2, scriptLen + 2 + 4))
    val fingerprint = bytes.slice(scriptLen + 2 + 4, scriptLen + 2 + 4 + 8)
    val contract = ESContract(bytes.slice(2, scriptLen), ScriptMeta(complexity, fingerprint))
    val amount = Longs.fromByteArray(bytes.slice(scriptLen + 2 + 4 + 8, scriptLen + 2 + 4 + 8 + 8))
    val idx = Ints.fromByteArray(bytes.takeRight(4))
    ScriptedAssetDirective(contract, amount, idx)
  }
}
