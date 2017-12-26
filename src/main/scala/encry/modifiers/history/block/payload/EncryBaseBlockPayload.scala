package encry.modifiers.history.block.payload

import encry.modifiers.{EncryTransactionCarryingPersistentModifier, ModifierWithDigest}
import scorex.core.ModifierId

trait EncryBaseBlockPayload
  extends EncryTransactionCarryingPersistentModifier with ModifierWithDigest {

  val headerId: ModifierId
}
