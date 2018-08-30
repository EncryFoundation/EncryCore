package encry.modifiers.history.block.payload

import encry.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.Transaction
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}
import encry.modifiers.state.box.EncryProposition

trait EncryBaseBlockPayload
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  val headerId: ModifierId
}
