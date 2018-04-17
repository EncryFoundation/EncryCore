package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.account.{Account, Address}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{AccountProposition, EncryProposition, PropositionSerializer}
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount

import scala.util.Try

case class AssetBox(override val proposition: EncryProposition,
                    override val nonce: Long,
                    override val amount: Amount)
  extends EncryBox[EncryProposition] with MonetaryBox {

  override type M = AssetBox

  override val typeId: BxTypeId = AssetBox.TypeId

  override def serializer: Serializer[M] = AssetBoxSerializer
}

object AssetBox {

  val TypeId: BxTypeId = 1.toByte

  implicit val jsonEncoder: Encoder[AssetBox] = (bx: AssetBox) => Map(
    "type" -> TypeId.asJson,
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce" -> bx.nonce.asJson,
    "value" -> bx.amount.asJson
  ).asJson

  def apply(address: Address, nonce: Long, amount: Amount): AssetBox =
    AssetBox(AccountProposition(address), nonce, amount)

  def apply(account: Account, nonce: Long, amount: Amount): AssetBox =
    AssetBox(AccountProposition(account), nonce, amount)
}

object AssetBoxSerializer extends Serializer[AssetBox] {

  override def toBytes(obj: AssetBox): Array[Byte] = {
    val propBytes = PropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {
    val accountPropositionLen = Shorts.fromByteArray(bytes.take(2))
    val iBytes = bytes.drop(2)
    val proposition = PropositionSerializer.parseBytes(iBytes.take(accountPropositionLen)).get
    val nonce = Longs.fromByteArray(iBytes.slice(accountPropositionLen, accountPropositionLen + 8))
    val amount = Longs.fromByteArray(iBytes.slice(accountPropositionLen + 8, accountPropositionLen + 16))
    AssetBox(proposition, nonce, amount)
  }
}
