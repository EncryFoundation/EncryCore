package encry.modifiers.history.block

import encry.modifiers.history.block.header.Header
import encry.modifiers.history.block.payload.EncryBaseBlockPayload
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.EncryProposition
import encry.modifiers.{EncryPersistentModifier, TransactionsCarryingPersistentNodeViewModifier}
import scala.util.Try

trait EncryBaseBlock
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction]
    with EncryPersistentModifier {

  val header: Header

  val payload: EncryBaseBlockPayload

  val toSeq: Seq[EncryPersistentModifier]

  def semanticValidity: Try[Unit]

  override def toString: String = s"<Block height=${header.height} timestamp=${header.timestamp} txQty=${payload.transactions.size} id=${header.encodedId}>"
}

