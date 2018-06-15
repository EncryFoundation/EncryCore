package encry.modifiers.history.block

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBaseBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.EncryProposition
import encry.utils.ScorexLogging
import scorex.core.TransactionsCarryingPersistentNodeViewModifier

import scala.util.Try

trait EncryBaseBlock
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, EncryBaseTransaction]
    with EncryPersistentModifier
    with ScorexLogging {

  val header: EncryBlockHeader

  val payload: EncryBaseBlockPayload

  val toSeq: Seq[EncryPersistentModifier]

  def semanticValidity: Try[Unit]

  override def toString: String = s"<Block timestamp=${header.timestamp} txQty=${payload.transactions.size} id=${header.encodedId}>"
}

