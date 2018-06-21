package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.Serializer
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{EncryProposition, EncryPropositionSerializer}
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey

import scala.util.Try

/** Holds the asset emission amount, reference to corresponding `AssetCreationBox` is required. */
case class AssetIssuingBox(override val proposition: EncryProposition,
                           override val nonce: Long,
                           override val amount: Amount,
                           creationBoxId: ADKey)
  extends EncryBox[EncryProposition] with MonetaryBox {

  override type M = AssetIssuingBox

  override val typeId: BxTypeId = AssetBox.TypeId

  override def serializer: Serializer[M] = AssetIssuingBoxSerializer

  override val tpe: Types.Product = Types.AssetIssuingBox

  override def asVal: PValue = PValue(convert, Types.DataBox)

  override def convert: PObject = {
    val fields = Map(
      "contractHash" -> PValue(proposition.contractHash, Types.PCollection.ofByte),
      "typeId" -> PValue(typeId, Types.PInt),
      "id" -> PValue(id, Types.PInt),
      "amount" -> PValue(amount, Types.PInt)
    )
    PObject(fields, tpe)
  }
}

object AssetIssuingBox {

  val TypeId: BxTypeId = 3.toByte

  implicit val jsonEncoder: Encoder[AssetIssuingBox] = (bx: AssetIssuingBox) => Map(
    "type" -> TypeId.asJson,
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce" -> bx.nonce.asJson
  ).asJson
}

object AssetIssuingBoxSerializer extends Serializer[AssetIssuingBox] {

  override def toBytes(obj: AssetIssuingBox): Array[Byte] = {
    val propBytes = EncryPropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount),
      obj.creationBoxId
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetIssuingBox] = Try {
    val propositionLen = Shorts.fromByteArray(bytes.take(2))
    val iBytes = bytes.drop(2)
    val proposition = EncryPropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val amount = Longs.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 8))
    val creationId = ADKey @@ bytes.takeRight(Constants.ModifierIdSize)
    AssetIssuingBox(proposition, nonce, amount, creationId)
  }
}
