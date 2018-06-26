package encry.modifiers.history.block.payload

import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}
import encry.ModifierId
import encry.modifiers.state.box.EncryProposition

trait EncryBaseBlockPayload
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, EncryBaseTransaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  val headerId: ModifierId
}
