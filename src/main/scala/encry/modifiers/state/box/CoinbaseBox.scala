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
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.{Success, Try}

// Now used currently.
case class CoinbaseBox(override val proposition: HeightProposition,
                       override val nonce: Long,
                       amount: Amount) extends EncryNoncedBox[HeightProposition] {

  override type M = CoinbaseBox

  override val typeId: BxTypeId = CoinbaseBox.typeId

  override val id: ADKey = ADKey @@ bxHash.updated(0, typeId) // 33 bytes!

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      Longs.toByteArray(nonce),
      Longs.toByteArray(amount)
    )
  )

  override def unlockTry(modifier: EncryTransaction, script: Option[String]): Try[Unit] = Success()

  override def serializer: SizedCompanionSerializer[M] = CoinbaseBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "proposition" -> s"Height proposition: ${proposition.height}".asJson,
    "nonce" -> nonce.asJson,
    "value" -> amount.asJson
  ).asJson
}

object CoinbaseBox {

  val typeId: BxTypeId = 3.toByte
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
