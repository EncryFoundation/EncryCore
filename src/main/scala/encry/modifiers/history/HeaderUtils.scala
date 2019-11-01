package encry.modifiers.history

import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object HeaderUtils {

  val TransactionsRootSize: Int = 32

  def syntacticallyValidity(header: Header, modifierIdSize: Int): ValidationResult = ModifierValidator.accumulateErrors
    .demand(header.modifierTypeId == Header.modifierTypeId,
      s"Modifier's type id should be ${Header.modifierTypeId}")
    .demand(header.id.size == modifierIdSize,
      s"Modifier's id should be $modifierIdSize bytes")
    .demand(header.parentId.size == modifierIdSize,
      s"Parent's id should be $modifierIdSize bytes")
    .demand(header.transactionsRoot.size == TransactionsRootSize,
      s"TransactionsRoot's size should be $TransactionsRootSize bytes")
    .result
}