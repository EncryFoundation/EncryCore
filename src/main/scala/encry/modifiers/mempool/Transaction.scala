package encry.modifiers.mempool

import encry.modifiers.NodeViewModifier
import encry.{ModifierId, ModifierTypeId}
import scorex.crypto.hash.Blake2b256

abstract class Transaction extends NodeViewModifier {
  override val modifierTypeId: ModifierTypeId = Transaction.ModifierTypeId

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = encry.ModifierId !@@ Blake2b256(messageToSign)
}

object Transaction { val ModifierTypeId: ModifierTypeId = encry.ModifierTypeId @@ 2.toByte }
