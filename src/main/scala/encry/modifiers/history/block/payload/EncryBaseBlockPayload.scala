package encry.modifiers.history.block.payload

import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.Transaction
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}
import encry.modifiers.state.box.EncryProposition

trait EncryBaseBlockPayload
   {

  val headerId: ModifierId
}
