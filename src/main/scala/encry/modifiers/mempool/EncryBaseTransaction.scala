package encry.modifiers.mempool

import encry.modifiers.mempool.EncryTransaction.TxTypeId
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.Secret
import scorex.core.{EphemerealNodeViewModifier, ModifierId, ModifierTypeId}
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Try

trait EncryBaseTransaction extends Transaction[Proposition] {
  override val modifierTypeId: ModifierTypeId = EncryBaseTransaction.ModifierTypeId

  val messageToSign: Array[Byte]

  // Used as `ModifierId`.
  val txHash: Digest32

  var signature: Signature25519

  val semanticValidity: Try[Unit]

  // TODO: Do we need tx Version?

  // Type of the transaction will be telling the abstract `dispatcher` how to treat particular Txn.
  val typeId: TxTypeId

  override lazy val id: ModifierId = ModifierId @@ Blake2b256(txHash)
}


object EncryBaseTransaction {
  val ModifierTypeId: scorex.core.ModifierTypeId = scorex.core.ModifierTypeId @@ 2.toByte
}
