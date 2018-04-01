package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADValue

import scala.util.Try

case class CoinbaseBox(override val proposition: HeightProposition,
                       override val nonce: Long,
                       override val amount: Amount) extends EncryBox[HeightProposition] with AmountCarryingBox {

  override type M = CoinbaseBox

  override val typeId: BxTypeId = CoinbaseBox.typeId

  override def serializer: SizedCompanionSerializer[M] = CoinbaseBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)
}

object CoinbaseBox {

  val typeId: BxTypeId = 0.toByte

  implicit val jsonEncoder: Encoder[CoinbaseBox] = (bx: CoinbaseBox) => Map(
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce" -> bx.nonce.asJson,
    "value" -> bx.amount.asJson
  ).asJson
}

object CoinbaseBoxSerializer extends SizedCompanionSerializer[CoinbaseBox] {

  val Size: Int = 20

  override def toBytes(obj: CoinbaseBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.serializer.toBytes(obj.proposition),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseBox] = Try {
    val proposition = HeightPropositionSerializer.parseBytes(bytes.slice(0, HeightPropositionSerializer.Size)).get
    val nonce = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size, HeightPropositionSerializer.Size + 8))
    val amount = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size + 8, HeightPropositionSerializer.Size + 16))
    CoinbaseBox(proposition, nonce, amount)
  }
}
