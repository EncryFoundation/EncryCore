package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.{Success, Try}

case class OpenBox(override val proposition: HeightProposition,
                   override val nonce: Long,
                   amount: Amount) extends EncryNoncedBox[HeightProposition] {

  override type M = OpenBox

  override val typeId: BxTypeId = OpenBox.typeId

  override val id: ADKey = ADKey @@ bxHash.updated(0, typeId)

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      Longs.toByteArray(nonce),
      Longs.toByteArray(amount)
    )
  )

  override def unlockTry(modifier: EncryTransaction, script: Option[String] = None): Try[Unit] = Success()

  override def serializer: SizedCompanionSerializer[M] = OpenBoxSerializer

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "proposition" -> s"Open after ${proposition.height}".asJson,
    "nonce" -> nonce.asJson,
    "value" -> value.asJson
  ).asJson
}

object OpenBox {

   val typeId: BxTypeId = 2.toByte
}

object OpenBoxSerializer extends SizedCompanionSerializer[OpenBox] {

  val Size: Int = 24

  override def toBytes(obj: OpenBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.serializer.toBytes(obj.proposition),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[OpenBox] = Try {
    val proposition = HeightPropositionSerializer.parseBytes(bytes.slice(0, HeightPropositionSerializer.Size)).get
    val nonce = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size, 8 + 8))
    val amount = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size + 8, 8 + 16))
    OpenBox(proposition, nonce, amount)
  }
}
