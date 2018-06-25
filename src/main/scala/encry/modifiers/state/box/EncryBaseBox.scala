package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.EncryProposition
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

  override def asVal: PValue = PValue(convert, tpe)

  def convert: PObject = {
    val fields = Map(
      "contractHash" -> PValue(proposition.contractHash, Types.PCollection.ofByte),
      "typeId" -> PValue(typeId, Types.PInt),
      "id" -> PValue(id, Types.PInt)
    )
    PObject(fields, tpe)
  }
}

object EncryBaseBox {

  implicit val jsonEncoder: Encoder[EncryBaseBox] = {
    case ab: AssetBox => AssetBox.jsonEncoder(ab)
    case db: DataBox => DataBox.jsonEncoder(db)
    case acb: AssetCreationBox => AssetCreationBox.jsonEncoder(acb)
    case aib: AssetIssuingBox => AssetIssuingBox.jsonEncoder(aib)
  }
}
