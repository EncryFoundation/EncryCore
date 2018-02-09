package encry.modifiers.mempool

import encry.modifiers.Signable25519
import encry.modifiers.mempool.EncryTransaction.TxTypeId
import encry.modifiers.state.box.{EncryBaseBox, OpenBox}
import encry.settings.Constants
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBaseTransaction extends Transaction[Proposition] with Signable25519 with ModifierWithSizeLimit {

  override val modifierTypeId: ModifierTypeId = EncryBaseTransaction.ModifierTypeId

  val txHash: Digest32

  val dataToSign: Array[Byte] = txHash

  val semanticValidity: Try[Unit]

  val typeId: TxTypeId

  // override lazy val id: ModifierId = ModifierId @@ (Array[Byte](typeId) ++ txHash)
  override lazy val id: ModifierId = ModifierId @@ txHash.updated(0, typeId)

  val fee: Long

  val timestamp: Long

  val feeBox: Option[OpenBox]

  // Holds IDs of the boxes to be opened.
  val useBoxes: IndexedSeq[ADKey]
  // Sequence of `Tx Outputs`.
  val newBoxes: Traversable[EncryBaseBox]

  val minimalFee: Float = Constants.feeMinAmount + Constants.txByteCost * length

  override def toString: String = s"<TX: type=$typeId id=${Base58.encode(id)}>"

  // Shadowed.
  override val messageToSign: Array[TxTypeId] = Array.fill(32)(1.toByte)
}

object EncryBaseTransaction {

  // Type of the transaction as the whole class of modifiers.
  val ModifierTypeId: scorex.core.ModifierTypeId = scorex.core.ModifierTypeId @@ 2.toByte
}
