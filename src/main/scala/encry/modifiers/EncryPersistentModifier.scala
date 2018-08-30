package encry.modifiers

import encry.CoreTaggedTypes.ModifierId

trait EncryPersistentModifier extends PersistentNodeViewModifier {

  override def parentId: ModifierId = null //scalastyle:ignore
}
