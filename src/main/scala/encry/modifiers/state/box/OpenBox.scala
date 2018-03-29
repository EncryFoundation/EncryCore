package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proof.Proof
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.{Encoder, Json}
import io.circe.syntax._
import scorex.core.transaction.box.Box.Amount

import scala.util.{Failure, Success, Try}

case class OpenBox(override val proposition: HeightProposition,
                   override val nonce: Long,
                   override val amount: Amount)
  extends EncryBox[HeightProposition] with AmountCarryingBox {

  override type M = OpenBox

  override val typeId: BxTypeId = OpenBox.typeId

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    if (proposition.height <= ctx.height) Success()
    else Failure(new Error("Unlock failed"))

  override def serializer: SizedCompanionSerializer[M] = OpenBoxSerializer
}

object OpenBox {

   val typeId: BxTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[OpenBox] = (bx: OpenBox) => Map(
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> s"Open after ${bx.proposition.height}".asJson,
    "nonce" -> bx.nonce.asJson,
    "value" -> bx.value.asJson
  ).asJson
}

object OpenBoxSerializer extends SizedCompanionSerializer[OpenBox] {

  val Size: Int = 20

  override def toBytes(obj: OpenBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.serializer.toBytes(obj.proposition),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount),
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[OpenBox] = Try {
    val proposition = HeightPropositionSerializer.parseBytes(bytes.slice(0, HeightPropositionSerializer.Size)).get
    val nonce = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size, HeightPropositionSerializer.Size + 8))
    val amount = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size + 8, HeightPropositionSerializer.Size + 16))
    OpenBox(proposition, nonce, amount)
  }
}
