package encry.modifiers.history

import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}
import scala.util.Try

object BlockUtils {

  def toSeq(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload)

  def transactions(block: Block): Seq[Transaction] = block.payload.txs
}