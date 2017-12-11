package encry.modifiers.history

import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import scorex.core.transaction.Transaction
import scorex.core.{ModifierId, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.transaction.box.proposition.Proposition

abstract class EncryBaseBlockPayload[P <: Proposition, TX <: Transaction[P]]
  extends EncryPersistentModifier with TransactionsCarryingPersistentNodeViewModifier[P, TX] with ModifierWithDigest {

  val headerId: ModifierId
  val txs: Seq[TX]
}
