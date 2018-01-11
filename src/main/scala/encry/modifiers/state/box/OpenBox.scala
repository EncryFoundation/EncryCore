package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.authds.ADValue
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class OpenBox(override val proposition: HeightProposition,
                   override val nonce: Long,
                   amount: Amount) extends EncryNoncedBox[HeightProposition] {

  override type M = OpenBox

  override val typeId: BxTypeId = OpenBox.typeId

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      Longs.toByteArray(nonce),
      Longs.toByteArray(amount)
    )
  )

  override def serializer: SizedCompanionSerializer[M] = OpenBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "proposition" -> s"Open after ${proposition.height}".asJson,
    "nonce" -> nonce.asJson,
    "value" -> value.asJson
  ).asJson
}

object OpenBox {

   val typeId: BxTypeId = 0.toByte
}

object OpenBoxSerializer extends SizedCompanionSerializer[OpenBox] {

  val Size: Int = 20

  override def toBytes(obj: OpenBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.serializer.toBytes(obj.proposition),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[OpenBox] = Try {
    val proposition = HeightPropositionSerializer.parseBytes(bytes.slice(0, 4)).get
    val nonce = Longs.fromByteArray(bytes.slice(Size, Size + 8))
    val amount = Longs.fromByteArray(bytes.slice(Size + 8, Size + 16))
    OpenBox(proposition, nonce, amount)
  }
}
