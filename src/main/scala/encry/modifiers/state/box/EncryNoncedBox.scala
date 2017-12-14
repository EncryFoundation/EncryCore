package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.state.box.body.BaseBoxBody
import encry.modifiers.state.box.proposition.AddressProposition
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256

trait EncryNoncedBox[P <: Proposition, BB <: BaseBoxBody] extends EncryBaseBox[P, BB] {

  val nonce: Long

  override val proposition: P

  override def equals(obj: Any): Boolean = obj match {
      // TODO: Implement `equals()` method in the `BoxBody`.
    case acc: EncryNoncedBox[P, BB] => (acc.id sameElements this.id) && acc.body == this.body
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}
