package encry.modifiers.history.block.payload

import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{ModifierId, TransactionsCarryingPersistentNodeViewModifier}

abstract class EncryBaseBlockPayload[P <: Proposition, TX <: Transaction[P]]
  extends EncryPersistentModifier with TransactionsCarryingPersistentNodeViewModifier[P, TX] with ModifierWithDigest {

  val headerId: ModifierId
  val txs: Seq[TX]
}
