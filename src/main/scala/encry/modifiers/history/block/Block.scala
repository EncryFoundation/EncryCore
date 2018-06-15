package encry.modifiers.history.block

import scorex.core.NodeViewModifier

object Block {
  type Timestamp = Long
  type Version = Byte
  type Height = Int

  val BlockIdLength: Int = NodeViewModifier.ModifierIdSize
}