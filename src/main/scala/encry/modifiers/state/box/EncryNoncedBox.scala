package encry.modifiers.state.box

import scorex.core.transaction.box.proposition.Proposition

trait EncryNoncedBox[P <: Proposition] extends EncryBox[P] {

  val nonce: Long

  override val proposition: P

  override def equals(obj: Any): Boolean = obj match {
      // TODO: Implement `equals()` method in the `BoxBody`.
    case bn: EncryNoncedBox[P] => bn.id sameElements this.id
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}
