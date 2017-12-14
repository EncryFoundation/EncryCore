package encry.modifiers.history.block.payload

import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import scorex.core.{EphemerealNodeViewModifier, ModifierId}

trait EncryBaseBlockPayload
  extends EncryPersistentModifier with ModifierWithDigest {

  val headerId: ModifierId

  val transactions: Seq[EphemerealNodeViewModifier]
}
