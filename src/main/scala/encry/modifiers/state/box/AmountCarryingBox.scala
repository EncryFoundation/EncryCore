package encry.modifiers.state.box

import encry.modifiers.mempool.EncryTransaction.Amount

trait AmountCarryingBox {

  val amount: Amount
}
