package encry.modifiers

import scorex.core.{ModifierId, PersistentNodeViewModifier}

trait EncryPersistentModifier extends PersistentNodeViewModifier {
  // TODO: Check weather this attr is necessary.
  override def parentId: ModifierId = null
}
