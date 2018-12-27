package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.Constants
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import scala.util.Try

case class TokenIssuingBox(override val proposition: EncryProposition,
                           override val nonce: Long,
                           override val amount: Amount,
                           tokenId: TokenId)
  extends EncryBox[EncryProposition] with MonetaryBox {

  override type M = TokenIssuingBox

  override val typeId: BxTypeId = AssetBox.TypeId

  override def serializer: Serializer[M] = AssetIssuingBoxSerializer

  override val tpe: Types.Product = Types.AssetIssuingBox

  override def asVal: PValue = PValue(asPrism, Types.DataBox)

  override def asPrism: PObject =
    PObject(baseFields ++ Map(
      "amount" -> PValue(amount, Types.PInt)
    ), tpe)
}

object TokenIssuingBox {

  type TokenId = Array[Byte]

  val TypeId: BxTypeId = 3.toByte

  implicit val jsonEncoder: Encoder[TokenIssuingBox] = (bx: TokenIssuingBox) => Map(
    "type"        -> TypeId.asJson,
    "id"          -> Algos.encode(bx.id).asJson,
    "tokenId"     -> Algos.encode(bx.tokenId).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce"       -> bx.nonce.asJson,
    "amount"      -> bx.amount.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[TokenIssuingBox] = (c: HCursor) => {
    for {
      proposition <- c.downField("proposition").as[EncryProposition]
      nonce       <- c.downField("nonce").as[Long]
      amount      <- c.downField("amount").as[Long]
      tokenId     <- c.downField("tokenId").as[TokenId]
    } yield TokenIssuingBox(
      proposition,
      nonce,
      amount,
      tokenId
    )
  }
}

object AssetIssuingBoxSerializer extends Serializer[TokenIssuingBox] {

  override def toBytes(obj: TokenIssuingBox): Array[Byte] = {
    val propBytes: Array[BxTypeId] = EncryPropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount),
      obj.tokenId
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[TokenIssuingBox] = Try {
    val propositionLen: Short = Shorts.fromByteArray(bytes.take(2))
    val iBytes: Array[BxTypeId] = bytes.drop(2)
    val proposition: EncryProposition = EncryPropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce: Amount = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val amount: Amount = Longs.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 8))
    val creationId: TokenId = bytes.takeRight(Constants.ModifierIdSize)
    TokenIssuingBox(proposition, nonce, amount, creationId)
  }
}
