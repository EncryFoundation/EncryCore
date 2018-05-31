package encry.view.state

import encry.modifiers.mempool.EncryBaseTransaction
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction._
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

trait StateFeature

trait TransactionValidator extends StateFeature {

  def isValid(tx: EncryBaseTransaction, allowedOutputDelta: Amount = 0L): Boolean = validate(tx, allowedOutputDelta).isSuccess

  def filterValid(txs: Seq[EncryBaseTransaction]): Seq[EncryBaseTransaction] = txs.filter(tx => isValid(tx))

  def validate(tx: EncryBaseTransaction, allowedOutputDelta: Amount = 0L): Try[Unit]
}

trait ModifierValidator[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}

trait BalanceSheet[P <: Proposition] extends StateFeature {
  def balance(id: P, height: Option[Int] = None): Long
}

trait AccountTransactionsHistory[P <: Proposition, TX <: Transaction[P]] extends StateFeature {
  def accountTransactions(id: P): Array[TX]
}
