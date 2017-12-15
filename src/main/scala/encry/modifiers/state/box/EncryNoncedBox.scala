package encry.modifiers.state.box

import encry.modifiers.state.box.body.BaseBoxBody
import scorex.core.transaction.box.proposition.Proposition

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
