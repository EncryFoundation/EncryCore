package encry.modifiers.history

import encry.settings.TestConstants
import org.encryfoundation.common.modifiers.history.Header
import encry.settings.EncryAppSettings.read.constants
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object HeaderUtils {

  def syntacticallyValidity(header: Header): ValidationResult = ModifierValidator.accumulateErrors
    .demand(header.modifierTypeId == Header.modifierTypeId,
      s"Modifier's type id should be ${Header.modifierTypeId}")
    .demand(header.id.size == constants.ModifierIdSize,
      s"Modifier's id should be ${constants.ModifierIdSize} bytes")
    .demand(header.parentId.size == constants.ModifierIdSize,
      s"Parent's id should be ${constants.ModifierIdSize} bytes")
    .demand(header.transactionsRoot.size == TestConstants.TransactionsRootSize,
      s"TransactionsRoot's size should be ${TestConstants.TransactionsRootSize} bytes")
    .result
}