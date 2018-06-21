package encry.modifiers

import scorex.core.ModifierId

trait EncryPersistentModifier extends PersistentNodeViewModifier {

  override def parentId: ModifierId = null //scalastyle:ignore
}
