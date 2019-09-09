package encry.modifiers.history

import encry.settings.TestConstants
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object HeaderUtils {

  def syntacticallyValidity(header: Header, modifierIdSize: Int): ValidationResult = ModifierValidator.accumulateErrors
    .demand(header.modifierTypeId == Header.modifierTypeId,
      s"Modifier's type id should be ${Header.modifierTypeId}")
    .demand(header.id.size == modifierIdSize,
      s"Modifier's id should be $modifierIdSize bytes")
    .demand(header.parentId.size == modifierIdSize,
      s"Parent's id should be $modifierIdSize bytes")
    .demand(header.transactionsRoot.size == TestConstants.TransactionsRootSize,
      s"TransactionsRoot's size should be ${TestConstants.TransactionsRootSize} bytes")
    .result
}