package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._

import scala.util.{Failure, Success, Try}

case class OpenBox(override val proposition: HeightProposition,
                   override val nonce: Long,
                   override val amount: Amount)
  extends EncryBox[HeightProposition] with AmountCarryingBox {

  override type M = OpenBox

  override val typeId: BxTypeId = OpenBox.typeId

  override def unlockTry(modifier: EncryBaseTransaction,
                         script: Option[String] = None)(implicit ctxOpt: Option[Context]): Try[Unit] =
    if (ctxOpt.isDefined && proposition.height <= ctxOpt.get.height) Success()
    else Failure(new Error("Unlock failed"))

  override def serializer: SizedCompanionSerializer[M] = OpenBoxSerializer

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> s"Open after ${proposition.height}".asJson,
    "nonce" -> nonce.asJson,
    "value" -> value.asJson
  ).asJson
}

object OpenBox {

   val typeId: BxTypeId = 2.toByte
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
