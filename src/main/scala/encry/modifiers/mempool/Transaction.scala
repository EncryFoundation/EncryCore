package encry.modifiers.mempool

import encry.modifiers.EphemerealNodeViewModifier
import encry.view.state.Proposition
import scorex.crypto.hash.Blake2b256
import encry.{ModifierId, ModifierTypeId}

abstract class Transaction[P <: Proposition] extends EphemerealNodeViewModifier {
  override val modifierTypeId: ModifierTypeId = Transaction.ModifierTypeId

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = encry.ModifierId @@ Blake2b256(messageToSign)
}

object Transaction {
  val ModifierTypeId: ModifierTypeId = encry.ModifierTypeId @@ 2.toByte
}