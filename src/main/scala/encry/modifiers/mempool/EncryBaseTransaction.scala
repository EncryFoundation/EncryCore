package encry.modifiers.mempool

import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{EphemerealNodeViewModifier, ModifierId, ModifierTypeId}
import scorex.crypto.hash.Blake2b256

import scala.util.Try

trait EncryBaseTransaction extends EphemerealNodeViewModifier {
  override val modifierTypeId: ModifierTypeId = EncryBaseTransaction.ModifierTypeId

  val messageToSign: Array[Byte]

  val semanticValidity: Try[Unit]

  override lazy val id: ModifierId = ModifierId @@ Blake2b256(messageToSign)
}


object EncryBaseTransaction {
  val ModifierTypeId: scorex.core.ModifierTypeId = scorex.core.ModifierTypeId @@ 2.toByte
}
