package encry.consensus

import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.encryfoundation.common.utils.constants.Constants

object EncrySupplyController {

  def supplyAt(height: Height, constants: Constants): Amount = {
    if (height > constants.EmissionEpochLength) (constants.InitialEmissionAmount * Math.pow(
      1 - constants.EmissionDecay,
      if (height % constants.EmissionEpochLength == 0) Math.round(height / constants.EmissionEpochLength) + 1
      else Math.round(height / constants.EmissionEpochLength)
    )).toLong
    else constants.InitialEmissionAmount
  }
}