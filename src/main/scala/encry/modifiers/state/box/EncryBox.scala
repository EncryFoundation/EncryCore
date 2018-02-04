package encry.modifiers.state.box

import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.Proposition

trait EncryBox[P <: Proposition] extends EncryBaseBox with JsonSerializable {

  override val proposition: P

  // Shadow redundant field from base class.
  override val value: Amount = 0L
}

object EncryBox {

  type BxTypeId = Byte

  val BoxIdSize = 32
}
