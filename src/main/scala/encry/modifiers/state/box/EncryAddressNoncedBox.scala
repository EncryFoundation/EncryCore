package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.state.box.body.BaseBoxBody
import encry.modifiers.state.box.proposition.AddressProposition
import scorex.core.ModifierId
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256

trait EncryAddressNoncedBox[BB <: BaseBoxBody] extends EncryBaseBox[AddressProposition, BB] {

  val nonce: Long

  override val proposition: AddressProposition

  override lazy val id: ADKey = ADKey @@ EncryAddressNoncedBox.idFromBox(proposition, nonce)

  override def equals(obj: Any): Boolean = obj match {
      // TODO: Implement `equals()` method in the `BoxBody`.
    case acc: EncryAddressNoncedBox[BB] => (acc.id sameElements this.id) && acc.body == this.body
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object EncryAddressNoncedBox {
  def idFromBox(addr: AddressProposition, nonce: Long): ModifierId =
    ModifierId @@ Blake2b256(addr.addrBytes ++ Longs.toByteArray(nonce))
}
