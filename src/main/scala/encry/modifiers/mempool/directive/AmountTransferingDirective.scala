package encry.modifiers.mempool.directive

import encry.modifiers.mempool.EncryTransaction.Amount

trait AmountTransferingDirective {

  val amount: Amount
}
