package encry.modifiers.state

import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{EphemerealNodeViewModifier, PersistentNodeViewModifier}

import scala.util.Try

trait StateFeature

trait TransactionValidator extends StateFeature {
  def isValid(tx: EphemerealNodeViewModifier): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[EphemerealNodeViewModifier]): Seq[EphemerealNodeViewModifier] = txs.filter(isValid)

  def validate(tx: EphemerealNodeViewModifier): Try[Unit]
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
