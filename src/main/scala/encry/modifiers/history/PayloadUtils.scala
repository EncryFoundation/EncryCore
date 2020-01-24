package encry.modifiers.history

import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object PayloadUtils {

  def syntacticallyValidity(payload: Payload, modifierIdSize: Int): ValidationResult = ModifierValidator.accumulateErrors
    .demand(payload.modifierTypeId == Payload.modifierTypeId,
      s"Modifier's type id should be ${Payload.modifierTypeId}")
    .demand(payload.headerId.size == modifierIdSize,
      s"Modifier's id should be $modifierIdSize bytes")
    //todo: Increase payload max size to 1.5 mb in common
    .demand(payload.bytes.length <= 1500000, //TestNetConstants.PayloadMaxSize,
                 "Incorrect payload size.")
    .result
}