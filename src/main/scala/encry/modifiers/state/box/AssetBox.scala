package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.serialization.Serializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import encry.modifiers.state.box.Box.Amount
import scorex.crypto.authds
import scorex.crypto.authds.ADKey
import supertagged.@@

import scala.util.Try

/**
  * Represents monetary asset of some type locked with some `proposition`.
  * `tokenIdOpt = None` if the asset is of intrinsic type.
  */
case class AssetBox(override val proposition: EncryProposition,
                    override val nonce: Long,
                    override val amount: Amount,
                    tokenIdOpt: Option[ADKey] = None)
  extends EncryBox[EncryProposition] with MonetaryBox {

  override type M = AssetBox

  override val typeId: BxTypeId = AssetBox.TypeId

  override def serializer: Serializer[M] = AssetBoxSerializer

  lazy val isIntrinsic: Boolean = tokenIdOpt.isEmpty

  override val tpe: Types.Product = Types.AssetBox

  override def asVal: PValue = PValue(convert, Types.AssetBox)

  override def convert: PObject = {
    val fields = Map(
      "contractHash" -> PValue(proposition.contractHash, Types.PCollection.ofByte),
      "typeId" -> PValue(typeId, Types.PInt),
      "id" -> PValue(id, Types.PInt),
      "amount" -> PValue(amount, Types.PInt),
      "tokenIdOpt" -> PValue(tokenIdOpt.flatMap(bytes => Some(bytes.untag(ADKey))), Types.POption(Types.PCollection.ofByte))
    )
    PObject(fields, tpe)
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
    "tokenId" -> bx.tokenIdOpt.map(id => Algos.encode(id)).asJson
  ).asJson
}

object AssetBoxSerializer extends Serializer[AssetBox] {

  override def toBytes(obj: AssetBox): Array[Byte] = {
    val propBytes = EncryPropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount),
      obj.tokenIdOpt.getOrElse(Array.empty)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {
    val propositionLen: Short = Shorts.fromByteArray(bytes.take(2))
    val iBytes: Array[BxTypeId] = bytes.drop(2)
    val proposition: EncryProposition = EncryPropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce: Amount = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val amount: Amount = Longs.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 8))
    val tokenIdOpt: Option[@@[Array[BxTypeId], authds.ADKey.Tag]] = if ((iBytes.length - (propositionLen + 8 + 8)) == Constants.ModifierIdSize) {
      Some(ADKey @@ iBytes.takeRight(Constants.ModifierIdSize))
    } else None
    AssetBox(proposition, nonce, amount, tokenIdOpt)
  }
}
