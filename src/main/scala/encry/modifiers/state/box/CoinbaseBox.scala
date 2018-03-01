package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proof.Proof
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADValue

import scala.util.{Failure, Success, Try}

case class CoinbaseBox(override val proposition: HeightProposition,
                       override val nonce: Long,
                       override val amount: Amount) extends EncryBox[HeightProposition] with AmountCarryingBox {

  override type M = CoinbaseBox

  override val typeId: BxTypeId = CoinbaseBox.typeId

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    if (proposition.height <= ctx.height) Success()
    else Failure(new Error("Unlock failed"))

  override def serializer: SizedCompanionSerializer[M] = CoinbaseBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> s"Height proposition: ${proposition.height}".asJson,
    "nonce" -> nonce.asJson,
    "value" -> amount.asJson
  ).asJson
}

object CoinbaseBox {

  val typeId: BxTypeId = 0.toByte
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
    val proposition = HeightPropositionSerializer.parseBytes(bytes.slice(0, 4)).get
    val nonce = Longs.fromByteArray(bytes.slice(Size, Size + 8))
    val amount = Longs.fromByteArray(bytes.slice(Size + 8, Size + 16))
    CoinbaseBox(proposition, nonce, amount)
  }
}
