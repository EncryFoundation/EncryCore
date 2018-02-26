package encry.modifiers.mempool.directive

import scorex.core.transaction.box.Box.Amount

trait AmountTransferingDirective {

  val amount: Amount
}
