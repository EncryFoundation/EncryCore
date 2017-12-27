package encry.modifiers.history.block.payload

import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{ModifierId, TransactionsCarryingPersistentNodeViewModifier}

trait EncryBaseBlockPayload
  extends TransactionsCarryingPersistentNodeViewModifier[Proposition, EncryBaseTransaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  val headerId: ModifierId
}
