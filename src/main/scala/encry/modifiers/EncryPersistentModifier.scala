package encry.modifiers

import encry.utils.CoreTaggedTypes.ModifierId

trait EncryPersistentModifier extends PersistentNodeViewModifier {

  override def parentId: ModifierId = null
}
