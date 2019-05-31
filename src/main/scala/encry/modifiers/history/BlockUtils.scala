package encry.modifiers.history

import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}
import scala.util.Try

object BlockUtils {

  def toSeq(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload) ++ block.adProofsOpt.toSeq

  def transactions(block: Block): Seq[Transaction] = block.payload.txs

  def semanticValidity(block: Block): Try[Unit] = validateSemantically(block).toTry

  def validateSemantically(block: Block): ValidationResult = ModifierValidator.accumulateErrors
    .demand(block.header.transactionsRoot != block.payload.digest, "Invalid payload root hash")
    .result

}