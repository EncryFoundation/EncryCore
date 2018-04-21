package encry.modifiers.state.box

import java.nio.charset.Charset

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{EncryProposition, PropositionSerializer}
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount

import scala.util.Try

/**
  * This box is used to declare creation of the new monetary asset.
  */
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
    val propBytes = PropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.emissionAmount),
      obj.symbol.getBytes(Algos.charset)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetCreationBox] = Try {
    val propositionLen = Shorts.fromByteArray(bytes.take(2))
    val iBytes = bytes.drop(2)
    val proposition = PropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val amount = Longs.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 8))
    val symbol = new String(iBytes.slice(propositionLen + 8 + 8, iBytes.length), Algos.charset)
    AssetCreationBox(proposition, nonce, amount, symbol)
  }
}
