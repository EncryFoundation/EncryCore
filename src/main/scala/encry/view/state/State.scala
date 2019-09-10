package encry.view.state

import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewErrors.ModifierApplyError
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.validation.ValidationResult

import scala.util.Try

trait State extends UtxoStateReader {

  def applyModifier(mod: PersistentModifier): Either[List[ModifierApplyError], State]

  def rollbackTo(version: VersionTag): Try[State]

  def validate(tx: Transaction, allowedOutputDelta: Amount = 0L): Either[ValidationResult, Transaction]

  def close(): Unit = storage.close()
}
