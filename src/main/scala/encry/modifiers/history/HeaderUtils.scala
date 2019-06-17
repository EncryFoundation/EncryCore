package encry.modifiers.history

import encry.settings.TestConstants
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object HeaderUtils {

  def syntacticallyValidity(header: Header): ValidationResult = ModifierValidator.accumulateErrors
    .demand(header.modifierTypeId == Header.modifierTypeId,
      s"Modifier's type id should be ${Header.modifierTypeId}")
    .demand(header.id.size == TestNetConstants.ModifierIdSize,
      s"Modifier's id should be ${TestNetConstants.ModifierIdSize} bytes")
    .demand(header.parentId.size == TestNetConstants.ModifierIdSize,
      s"Parent's id should be ${TestNetConstants.ModifierIdSize} bytes")
    .demand(header.stateRoot.size == TestConstants.StateRootSize,
      s"StateRoot's size should be ${TestConstants.StateRootSize} bytes")
    .demand(header.adProofsRoot.size == TestConstants.AdProofsRootSize,
      s"AdProofsRoot's size should be ${TestConstants.AdProofsRootSize} bytes")
    .demand(header.transactionsRoot.size == TestConstants.TransactionsRootSize,
      s"TransactionsRoot's size should be ${TestConstants.TransactionsRootSize} bytes")
    .result
}