package encry.modifiers.history

import encry.settings.TestConstants
import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object PayloadUtils {

  def syntacticallyValidity(payload: Payload): ValidationResult = ModifierValidator.accumulateErrors
    .demand(payload.modifierTypeId == Payload.modifierTypeId, "Modifier's type id should be 102")
    .demand(payload.headerId.size == TestNetConstants.ModifierIdSize, "Modifier's id should be 32 bytes")
    .demand(payload.serializer.toBytes(payload).length < TestConstants.PayloadMaxSize,
      "Payload's max size should be less than boundary value")
    .result

  def semanticValidity(payload: Payload): ValidationResult = ModifierValidator.accumulateErrors
    .demand(payload.txs.nonEmpty, "Should contain at least 1 coinbase-like transaction")
    .result

}