package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{EncryProposition, HeightProposition, OpenProposition}
import encry.settings.Algos
import io.circe.Encoder
import scorex.core.transaction.box.Box
import scorex.crypto.authds.ADKey

trait EncryBaseBox extends Box[EncryProposition] {

  val typeId: BxTypeId

  val nonce: Long

  override lazy val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce)).updated(0, typeId) // 32 bytes!

  override def toString: String = s"<Box type=:$typeId id=:${Algos.encode(id)}>"

  def isCoinbase: Boolean = this.isInstanceOf[CoinbaseBox]
  def isAmountCarrying: Boolean = this.isInstanceOf[AmountCarryingBox]
  def isOpen: Boolean = this.proposition.isInstanceOf[OpenProposition.type]
  def isHeightLocked: Boolean = this.proposition.isInstanceOf[HeightProposition]
}

object EncryBaseBox {

  implicit val jsonEncoder: Encoder[EncryBaseBox] = {
    case ab: AssetBox => AssetBox.jsonEncoder(ab)
    case cb: CoinbaseBox => CoinbaseBox.jsonEncoder(cb)
    case pkb: PubKeyInfoBox => PubKeyInfoBox.jsonEncoder(pkb)
  }
}
