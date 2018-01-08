package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.crypto.Address
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.serializers.BoxCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.authds.ADValue
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class AssetBox(override val proposition: AddressProposition,
                    override val nonce: Long,
                    amount: Amount) extends EncryNoncedBox[AddressProposition] {

  override type M = AssetBox

  override val typeId: BxTypeId = 1.toByte

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      proposition.bytes,
      Longs.toByteArray(nonce),
      Longs.toByteArray(amount)
    )
  )

  override def serializer: BoxCompanionSerializer[AssetBox] = AssetBoxSerializer

  override lazy val bytes: ADValue = ADValue @@ serializer.toBytes(this)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "proposition" -> proposition.address.toString.asJson,
    "nonce" -> nonce.asJson,
    "value" -> value.asJson
  ).asJson
}

object AssetBoxSerializer extends BoxCompanionSerializer[AssetBox] {

  val Length: Int = 37

  override def toBytes(obj: AssetBox): Array[Byte] = {
    Bytes.concat(
      AddressProposition.addrBytes(obj.proposition.address),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {
    val proposition = new AddressProposition(Address @@ Base58.encode(bytes.slice(0, Length)))
    val nonce = Longs.fromByteArray(bytes.slice(Length, Length + 8))
    val amount = Longs.fromByteArray(bytes.slice(Length + 8, Length + 16))
    AssetBox(proposition, nonce, amount)
  }
}
