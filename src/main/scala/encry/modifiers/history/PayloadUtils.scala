package encry.modifiers.history

import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object PayloadUtils {

  //todo add payload size validation
  def syntacticallyValidity(payload: Payload): ValidationResult = ModifierValidator.accumulateErrors
    .demand(payload.modifierTypeId == Payload.modifierTypeId,
      s"Modifier's type id should be ${Payload.modifierTypeId}")
    .demand(payload.headerId.size == TestNetConstants.ModifierIdSize,
      s"Modifier's id should be ${TestNetConstants.ModifierIdSize} bytes")
    .result
}