package encry.modifiers

import encry.ModifierId

trait EncryPersistentModifier extends PersistentNodeViewModifier {

  override def parentId: ModifierId = null //scalastyle:ignore
}
