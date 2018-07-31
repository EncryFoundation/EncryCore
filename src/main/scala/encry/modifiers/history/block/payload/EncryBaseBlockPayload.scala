package encry.modifiers.history.block.payload

import encry.modifiers.mempool.BaseTransaction
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}
import encry.ModifierId
import encry.modifiers.state.box.EncryProposition

trait EncryBaseBlockPayload
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, BaseTransaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  val headerId: ModifierId
}
