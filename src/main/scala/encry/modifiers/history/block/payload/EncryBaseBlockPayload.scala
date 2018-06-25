package encry.modifiers.history.block.payload

import encry.ModifierId
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}

trait EncryBaseBlockPayload
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, EncryBaseTransaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  val headerId: ModifierId
}
