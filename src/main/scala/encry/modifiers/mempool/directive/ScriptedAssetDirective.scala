package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import encrywm.common.{EncryContract, ScriptMeta}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class ScriptedAssetDirective(contract: CompiledContract,
                                  amount: Amount,
                                  tokenIdOpt: Option[ADKey] = None) extends Directive {

  override type M = ScriptedAssetDirective

  override val typeId: DTypeId = ScriptedAssetDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(AssetBox(EncryProposition(contract), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount))

  override val cost: Amount = 4

  override lazy val isValid: Boolean = amount > 0

  override def serializer: Serializer[M] = ScriptedAssetDirectiveSerializer

  lazy val isIntrinsic: Boolean = tokenIdOpt.isEmpty
}

object ScriptedAssetDirective {

  val TypeId: DTypeId = 3.toByte

  implicit val jsonEncoder: Encoder[ScriptedAssetDirective] = (d: ScriptedAssetDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "contract" -> Base58.encode(d.contract.bytes).asJson,
    "amount" -> d.amount.asJson,
    "tokenId" -> d.tokenIdOpt.map(id => Algos.encode(id)).getOrElse("null").asJson
  ).asJson

  implicit val jsonDecoder: Decoder[ScriptedAssetDirective] = (c: HCursor) => for {
    contractBytes <- c.downField("contract").as[String]
    amount <- c.downField("amount").as[Long]
    tokenIdOpt <- c.downField("tokenId").as[Option[String]]
  } yield {
    val contract: CompiledContract = Algos.decode(contractBytes).flatMap(CompiledContractSerializer.parseBytes)
      .getOrElse(throw new Exception("Decoding failed"))
    ScriptedAssetDirective(contract, amount, tokenIdOpt.map(Algos.decode).flatten)
  }
}

object ScriptedAssetDirectiveSerializer extends Serializer[ScriptedAssetDirective] {

  override def toBytes(obj: ScriptedAssetDirective): Array[Byte] =
    Bytes.concat(
      Shorts.toByteArray(obj.contract.bytes.length.toShort),
      obj.contract.bytes,
      Longs.toByteArray(obj.amount),
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
