package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{OpenProposition, OpenPropositionSerializer}
import encry.modifiers.state.box.serializers.BoxCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.authds.ADValue
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class OpenBox(override val nonce: Long,
                   amount: Amount) extends EncryNoncedBox[OpenProposition.type] {

  override type M = OpenBox

  override val proposition: OpenProposition.type = OpenProposition

  override val typeId: BxTypeId = 0.toByte

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      Longs.toByteArray(nonce),
      Longs.toByteArray(amount)
    )
  )

  override def serializer: BoxCompanionSerializer[M] = OpenBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "proposition" -> "Open".asJson,
    "nonce" -> nonce.asJson,
    "value" -> value.asJson
  ).asJson
}

object OpenBoxSerializer extends BoxCompanionSerializer[OpenBox] {

  val Length: Int = 17

  override def toBytes(obj: OpenBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.serializer.toBytes(obj.proposition),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[OpenBox] = Try {
    val _ = OpenPropositionSerializer.parseBytes(bytes.slice(0, 1))
    val nonce = Longs.fromByteArray(bytes.slice(Length, Length + 8))
    val amount = Longs.fromByteArray(bytes.slice(Length + 8, Length + 16))
    OpenBox(nonce, amount)
  }
}
