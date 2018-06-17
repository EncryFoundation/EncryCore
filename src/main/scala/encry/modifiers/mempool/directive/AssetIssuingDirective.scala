package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import encry.account.Account
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.state.box.{AssetCreationBox, AssetIssuingBox, EncryBaseBox}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.PublicKey
import scorex.utils.Random

import scala.util.Try

case class AssetIssuingDirective(contract: CompiledContract,
                                 amount: Amount,
                                 symbol: String) extends Directive {

  override type M = AssetIssuingDirective

  override val typeId: DTypeId = AssetIssuingDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] = {
    val assetCreationBox = AssetCreationBox(EncryProposition.accountLock(Account(PublicKey @@ Random.randomBytes(32))),
      Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx) ++ Ints.toByteArray(1)), amount, symbol)
    Seq(
      assetCreationBox,
      AssetIssuingBox(EncryProposition(contract),
        Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx) ++ Ints.toByteArray(2)), amount, assetCreationBox.id)
    )
  }

  override val cost: Amount = 20

  override lazy val isValid: Boolean = amount > 0 && symbol.length <= Constants.Chain.TokenSymbolMaxLength

  override def serializer: Serializer[M] = AssetIssuingDirectiveSerializer
}

object AssetIssuingDirective {

  val TypeId: DTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[AssetIssuingDirective] = (d: AssetIssuingDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "contract" -> Base58.encode(d.contract.bytes).asJson,
    "amount" -> d.amount.asJson,
    "symbol" -> d.symbol.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[AssetIssuingDirective] = (c: HCursor) => {
    for {
      contractBytes <- c.downField("contract").as[String]
      amount <- c.downField("amount").as[Long]
      symbol <- c.downField("symbol").as[String]
    } yield {
      val contract: CompiledContract = Algos.decode(contractBytes).flatMap(CompiledContractSerializer.parseBytes)
        .getOrElse(throw new Exception("Decoding failed"))
      AssetIssuingDirective(contract, amount, symbol)
    }
  }
}

object AssetIssuingDirectiveSerializer extends Serializer[AssetIssuingDirective] {

  override def toBytes(obj: AssetIssuingDirective): Array[Byte] =
    Bytes.concat(
      Shorts.toByteArray(obj.contract.bytes.length.toShort),
      obj.contract.bytes,
      Longs.toByteArray(obj.amount),
      obj.symbol.getBytes(Algos.charset)
    )

  override def parseBytes(bytes: Array[Byte]): Try[AssetIssuingDirective] = {
    val scriptLen = Shorts.fromByteArray(bytes.take(2))
    CompiledContractSerializer.parseBytes(bytes.slice(2, scriptLen + 2)).map { contract =>
      val amount = Longs.fromByteArray(bytes.slice(scriptLen + 2, scriptLen + 2 + 8))
      val symbol = new String(bytes.slice(scriptLen + 2 + 8, bytes.length), Algos.charset)
      AssetIssuingDirective(contract, amount, symbol)
    }
  }
}
