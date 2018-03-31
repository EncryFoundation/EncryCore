package encry.modifiers.history.block.payload

import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import scorex.core.{ModifierId, TransactionsCarryingPersistentNodeViewModifier}

trait EncryBaseBlockPayload
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, EncryBaseTransaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  val headerId: ModifierId
}
