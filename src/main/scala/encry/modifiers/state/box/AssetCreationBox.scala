package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.serialization.Serializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._

import scala.util.Try

case class AssetCreationBox(override val proposition: EncryProposition,
                            override val nonce: Long,
                            emissionAmount: Amount,
                            symbol: String) extends EncryBox[EncryProposition] {

  override type M = AssetCreationBox
  override val typeId: BxTypeId = AssetBox.TypeId

  override def serializer: Serializer[M] = AssetCreationBoxSerializer
}

object AssetCreationBox {

  val TypeId: BxTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[AssetCreationBox] = (bx: AssetCreationBox) => Map(
    "type" -> TypeId.asJson,
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce" -> bx.nonce.asJson,
    "emissionAmount" -> bx.emissionAmount.asJson,
    "symbol" -> bx.symbol.asJson
  ).asJson
}

object AssetCreationBoxSerializer extends Serializer[AssetCreationBox] {

  override def toBytes(obj: AssetCreationBox): Array[Byte] = {
    val propBytes = EncryPropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.emissionAmount),
      obj.symbol.getBytes(Algos.charset)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetCreationBox] = Try {
    val propositionLen: Short = Shorts.fromByteArray(bytes.take(2))
    val iBytes: Array[BxTypeId] = bytes.drop(2)
    val proposition: EncryProposition = EncryPropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce: Amount = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val amount: Amount = Longs.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 8))
    val symbol: String = new String(iBytes.slice(propositionLen + 8 + 8, iBytes.length), Algos.charset)
    AssetCreationBox(proposition, nonce, amount, symbol)
  }
}