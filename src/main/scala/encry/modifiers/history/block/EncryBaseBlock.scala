package encry.modifiers.history.block

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBaseBlockPayload
import scorex.core.TransactionsCarryingPersistentNodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

trait EncryBaseBlock[P <: Proposition, TX <: Transaction[P], BP <: EncryBaseBlockPayload[P, TX]]
  extends EncryPersistentModifier with TransactionsCarryingPersistentNodeViewModifier[P, TX] {

  val header: EncryBlockHeader

  val payload: BP

  val toSeq: Seq[EncryPersistentModifier]

}

