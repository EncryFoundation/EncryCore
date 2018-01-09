package encry.modifiers.mempool

import encry.modifiers.mempool.EncryTransaction.TxTypeId
import encry.modifiers.state.box.EncryBaseBox
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBaseTransaction extends Transaction[Proposition] {

  override val modifierTypeId: ModifierTypeId = EncryBaseTransaction.ModifierTypeId

  val messageToSign: Array[Byte]

  val txHash: Digest32

  var signature: Signature25519

  val semanticValidity: Try[Unit]

  // TODO: Do we need tx Version?

  // TODO: Do we need `typeId` here?
  val typeId: TxTypeId

  override lazy val id: ModifierId = ModifierId @@ (Array[Byte](typeId) ++ txHash)

  val fee: Long

  val timestamp: Long

  val length: Int

  // `BoxUnlocker` holds ID and Key of the box to open (Sequence of `Tx Inputs` + Keys to unlock them).
  val unlockers: Traversable[BoxUnlocker[_]]
  // Sequence of `Tx Outputs`.
  val newBoxes: Traversable[EncryBaseBox]
}

object EncryBaseTransaction {

  val ModifierTypeId: scorex.core.ModifierTypeId = scorex.core.ModifierTypeId @@ 2.toByte
}
