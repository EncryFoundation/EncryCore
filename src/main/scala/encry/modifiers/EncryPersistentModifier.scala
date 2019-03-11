package encry.modifiers

import encry.utils.CoreTaggedTypes.ModifierId

trait EncryPersistentModifier extends PersistentNodeViewModifier {

  ///TODO CHECK NULL
  override def parentId: ModifierId = null
}
