package encry.modifiers.state.box

import scorex.core.transaction.box.Box.Amount

trait AmountCarryingBox {

  val amount: Amount
}
