package encry.modifiers

import scorex.core.{ModifierId, PersistentNodeViewModifier}

trait EncryPersistentModifier extends PersistentNodeViewModifier {

  override def parentId: ModifierId = null //scalastyle:ignore
}
