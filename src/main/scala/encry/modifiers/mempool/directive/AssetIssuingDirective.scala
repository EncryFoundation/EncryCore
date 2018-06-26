package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account.Account
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.serialization.Serializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetCreationBox, AssetIssuingBox, EncryBaseBox, EncryProposition}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.PublicKey
import scorex.utils.Random

import scala.util.Try

case class AssetIssuingDirective(contractHash: ContractHash, amount: Amount, symbol: String) extends Directive {

  override type M = AssetIssuingDirective
  override val typeId: DTypeId = AssetIssuingDirective.TypeId
  override lazy val isValid: Boolean = amount > 0 && symbol.length <= Constants.Chain.TokenSymbolMaxLength

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] = {
    val assetCreationBox = AssetCreationBox(EncryProposition.accountLock(Account(PublicKey @@ Random.randomBytes(32))),
      Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx) ++ Ints.toByteArray(1)), amount, symbol)
    Seq(
      assetCreationBox,
      AssetIssuingBox(EncryProposition(contractHash),
        Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx) ++ Ints.toByteArray(2)), amount, assetCreationBox.id)
    )
  }

  override def serializer: Serializer[M] = AssetIssuingDirectiveSerializer
}

object AssetIssuingDirective {

  val TypeId: DTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[AssetIssuingDirective] = (d: AssetIssuingDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "contractHash" -> Base58.encode(d.contractHash).asJson,
    "amount" -> d.amount.asJson,
    "symbol" -> d.symbol.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[AssetIssuingDirective] = (c: HCursor) => {
    for {
      contractHash <- c.downField("contractHash").as[String]
      amount <- c.downField("amount").as[Long]
      symbol <- c.downField("symbol").as[String]
    } yield Algos.decode(contractHash)
      .map(ch => AssetIssuingDirective(ch, amount, symbol))
      .getOrElse(throw new Exception("Decoding failed"))
  }
}

object AssetIssuingDirectiveSerializer extends Serializer[AssetIssuingDirective] {

  override def toBytes(obj: AssetIssuingDirective): Array[Byte] =
    Bytes.concat(
      obj.contractHash,
      Longs.toByteArray(obj.amount),
      obj.symbol.getBytes(Algos.charset)
    )

  override def parseBytes(bytes: Array[Byte]): Try[AssetIssuingDirective] = Try {
    val contractHash: ContractHash = bytes.take(Constants.DigestLength)
    val amount: Amount = Longs.fromByteArray(bytes.slice(Constants.DigestLength, Constants.DigestLength + 8))
    val symbol: String = new String(bytes.slice(Constants.DigestLength + 8, bytes.length), Algos.charset)
    AssetIssuingDirective(contractHash, amount, symbol)
  }
}