package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{ContractProposition, EncryProposition, HeightProposition, OpenProposition}
import encry.settings.Algos
import encrywm.lang.backend.env.{ESEnvConvertable, ESObject, ESValue}
import encrywm.lib.Types
import io.circe.Encoder
import scorex.core.transaction.box.Box
import scorex.crypto.authds.ADKey

trait EncryBaseBox extends Box[EncryProposition] with ESEnvConvertable {

  val typeId: BxTypeId

  val nonce: Long

  override lazy val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce)).updated(0, typeId) // 32 bytes!

  def isCoinbase: Boolean = this.isInstanceOf[CoinbaseBox]
  def isAmountCarrying: Boolean = this.isInstanceOf[MonetaryBox]
  def isOpen: Boolean = this.proposition.isInstanceOf[OpenProposition.type]
  def isHeightLocked: Boolean = this.proposition.isInstanceOf[HeightProposition]
  def isLockedByScript: Boolean = this.proposition.isInstanceOf[ContractProposition]

  override val esType: Types.ESProduct = Types.ESBox

  override def asVal: ESValue = ESValue(Types.ESBox.ident.toLowerCase, Types.ESBox)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "proposition" -> ESValue("proposition", Types.ESProposition)(proposition.convert),
      "typeId" -> ESValue("typeId", Types.ESInt)(typeId),
      "id" -> ESValue("id", Types.ESByteVector)(id)
    )
    ESObject(Types.ESBox.ident, fields, esType)
  }
}

object EncryBaseBox {

  implicit val jsonEncoder: Encoder[EncryBaseBox] = {
    case ab: AssetBox => AssetBox.jsonEncoder(ab)
    case cb: CoinbaseBox => CoinbaseBox.jsonEncoder(cb)
  }
}
