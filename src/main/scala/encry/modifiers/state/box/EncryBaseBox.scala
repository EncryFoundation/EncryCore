package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.Algos
import io.circe.Encoder
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import org.encryfoundation.prismlang.core.{PConvertible, Types}
import scorex.crypto.authds.ADKey

trait EncryBaseBox extends Box[EncryProposition] with PConvertible {

  val typeId: BxTypeId

  val nonce: Long

  override lazy val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce)).updated(0, typeId)

  def isAmountCarrying: Boolean = this.isInstanceOf[MonetaryBox]

  override val tpe: Types.Product = Types.EncryBox

  override def asVal: PValue = PValue(asPrism, tpe)

  def asPrism: PObject =
    PObject(Map(
      "contractHash" -> PValue(proposition.contractHash, Types.PCollection.ofByte),
      "typeId" -> PValue(typeId.toLong, Types.PInt),
      "id" -> PValue(id, Types.PCollection.ofByte)
    ), tpe)
}

object EncryBaseBox {

  implicit val jsonEncoder: Encoder[EncryBaseBox] = {
    case ab: AssetBox => AssetBox.jsonEncoder(ab)
    case db: DataBox => DataBox.jsonEncoder(db)
    case aib: TokenIssuingBox => TokenIssuingBox.jsonEncoder(aib)
  }
}
