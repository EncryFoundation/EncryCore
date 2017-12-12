package encry.modifiers.history

import encry.modifiers.EncryPersistentModifier
import scorex.core.TransactionsCarryingPersistentNodeViewModifier
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.Transaction

// TODO: How to implement `FullBlock` carrying different types of Txn?
trait EncryBaseBlock[P <: Proposition, TX <: Transaction[P], BP <: EncryBaseBlockPayload[P, TX]]
  extends EncryPersistentModifier with TransactionsCarryingPersistentNodeViewModifier[P, TX] {

  val header: EncryBlockHeader

  val blockPayload: BP

  val toSeq: Seq[EncryPersistentModifier]

}

