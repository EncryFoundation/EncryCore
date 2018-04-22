package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.account.{Account, Address}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{AccountProposition, EncryProposition, PropositionSerializer}
import encry.settings.{Algos, Constants}
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.{ESByteVector, ESLong, ESOption}
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey

import scala.util.Try

/**
  * Represents monetary asset of some type locked with some `proposition`.
  * `tokenIdOpt = None` if the asset is of intrinsic type.
  */
case class AssetBox(override val proposition: EncryProposition,
                    override val nonce: Long,
                    override val amount: Amount,
                    tokenIdOpt: Option[ADKey] = None)
  extends EncryBox[EncryProposition] with MonetaryBox{

  override type M = AssetBox

  override val typeId: BxTypeId = AssetBox.TypeId

  override def serializer: Serializer[M] = AssetBoxSerializer

  lazy val isIntrinsic: Boolean = tokenIdOpt.isEmpty

  override val esType: Types.ESProduct = Types.AssetBox

  override def asVal: ESValue = ESValue(Types.AssetBox.ident.toLowerCase, Types.AssetBox)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "amount" -> ESValue("amount", ESLong)(amount),
      "tokenIdOpt" -> ESValue("tokenIdOpt", ESOption(ESByteVector))(tokenIdOpt.flatMap(bytes => Some(bytes.untag(ADKey))))
    )
    ESObject(Types.AssetBox.ident, fields, esType)
  }
}

object AssetBox {

  val TypeId: BxTypeId = 1.toByte

  implicit val jsonEncoder: Encoder[AssetBox] = (bx: AssetBox) => Map(
    "type" -> TypeId.asJson,
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce" -> bx.nonce.asJson,
    "value" -> bx.amount.asJson,
    "tokenId" -> bx.tokenIdOpt.map(id => Algos.encode(id)).getOrElse("null").asJson
  ).asJson

  def apply(address: Address, nonce: Long, amount: Amount, tokenIdOpt: Option[ADKey]): AssetBox =
    AssetBox(AccountProposition(address), nonce, amount, tokenIdOpt)

  def apply(account: Account, nonce: Long, amount: Amount, tokenIdOpt: Option[ADKey]): AssetBox =
    AssetBox(AccountProposition(account), nonce, amount, tokenIdOpt)
}

object AssetBoxSerializer extends Serializer[AssetBox] {

  override def toBytes(obj: AssetBox): Array[Byte] = {
    val propBytes = PropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount),
      obj.tokenIdOpt.getOrElse(Array.empty)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {
    val propositionLen = Shorts.fromByteArray(bytes.take(2))
    val iBytes = bytes.drop(2)
    val proposition = PropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val amount = Longs.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 8))
    val tokenIdOpt = if ((iBytes.length - (propositionLen + 8 + 8)) == Constants.ModifierIdSize) {
      Some(ADKey @@ iBytes.takeRight(Constants.ModifierIdSize))
    } else None
    AssetBox(proposition, nonce, amount, tokenIdOpt)
  }
}
