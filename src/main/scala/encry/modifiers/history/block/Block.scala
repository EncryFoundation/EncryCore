package encry.modifiers.history.block

import encry.modifiers.NodeViewModifier

object Block {
  type Timestamp = Long
  type Version = Byte
  type Height = Int

  val BlockIdLength: Int = NodeViewModifier.ModifierIdSize
}