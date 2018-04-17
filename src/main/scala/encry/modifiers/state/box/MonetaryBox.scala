package encry.modifiers.state.box

import scorex.core.transaction.box.Box.Amount

trait MonetaryBox extends EncryBaseBox {

  val amount: Amount
}
