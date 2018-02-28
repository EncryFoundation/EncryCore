package encry.modifiers.state.box

import scorex.core.transaction.box.Box.Amount

trait AmountCarryingBox extends EncryBaseBox {

  val amount: Amount
}
