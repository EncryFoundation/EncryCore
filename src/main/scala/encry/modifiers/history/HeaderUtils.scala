package encry.modifiers.history

import com.google.common.primitives.{Ints, Longs}
import encry.settings.TestConstants
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object HeaderUtils {

  def syntacticallyValidity(header: Header): ValidationResult = ModifierValidator.accumulateErrors
    .demand(header.modifierTypeId == Header.modifierTypeId, "Modifier's type id should be 101")
    .demand(header.id.size == TestNetConstants.ModifierIdSize, "Modifier's id should be 32 bytes")
    .demand(header.parentId.size == TestNetConstants.ModifierIdSize, "Parent's id should be 32 bytes")
    .demand(header.stateRoot.size == TestConstants.StateRootSize, "StateRoot's size should be 33 bytes")
    .demand(header.adProofsRoot.size == TestConstants.AdProofsRootSize, "AdProofsRoot's size should be 32 bytes")
    .demand(header.transactionsRoot.size == TestConstants.TransactionsRootSize, "TransactionsRoot's size should be 32 bytes")
    .demand(header.timestamp < System.currentTimeMillis(), "Header shouldn't be from the future")
    .demand(headerBytesSize(header) <= TestConstants.HeaderMaxSize, "Header's byte size should be less than 322 bytes")
    .result

  def semanticValidity(header: Header): ValidationResult = ModifierValidator.accumulateErrors
    .demand(header.height > TestNetConstants.GenesisHeight, "Header's height should be more than 0")
    .demand(header.difficulty >= TestNetConstants.InitialDifficulty, "Difficulty should be more than 0")
    .demand(header.equihashSolution.ints.nonEmpty, "Equihash solution shouldn't be empty")
    .result

  def headerBytesSize(header: Header): Int = 1 + header.id.size + header.parentId.size +
    header.adProofsRoot.size + header.stateRoot.size + header.transactionsRoot.size +
      Longs.toByteArray(header.timestamp).length + Ints.toByteArray(header.height).length +
    Longs.toByteArray(header.nonce).length

}