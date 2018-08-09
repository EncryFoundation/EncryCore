package encry.view.state

import encry.VersionTag
import encry.modifiers.PersistentNodeViewModifier
import encry.modifiers.mempool.Transaction
import org.encryfoundation.common.transaction.Proposition
import scala.util.Try

trait MinimalState[M <: PersistentNodeViewModifier, MS <: MinimalState[M, MS]] extends StateReader {
  self: MS =>

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  def getReader: StateReader = this

}

trait StateFeature

trait TransactionValidation[P <: Proposition, TX <: Transaction] extends StateFeature {
  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def validate(tx: TX): Try[Unit]
}

trait ModifierValidation[M <: PersistentNodeViewModifier] extends StateFeature {
  def validate(mod: M): Try[Unit]
}