package encry.modifiers.history.block

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBaseBlockPayload
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.EncryProposition
import encry.modifiers.{EncryPersistentModifier, TransactionsCarryingPersistentNodeViewModifier}
import encry.utils.Logging
import scala.util.Try

trait EncryBaseBlock
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction]
    with EncryPersistentModifier
    with Logging {

  val header: EncryBlockHeader

  val payload: EncryBaseBlockPayload

  val toSeq: Seq[EncryPersistentModifier]

  def semanticValidity: Try[Unit]

  override def toString: String = s"<Block height=${header.height} timestamp=${header.timestamp} txQty=${payload.transactions.size} id=${header.encodedId}>"
}

