package encry.modifiers.state.box

import encry.modifiers.state.box.Box.Amount

trait MonetaryBox extends EncryBaseBox {

  val amount: Amount
}
