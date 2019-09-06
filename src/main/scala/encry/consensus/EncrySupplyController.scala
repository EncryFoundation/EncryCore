package encry.consensus

import encry.settings.EncryAppSettings.settings.constants
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height > constants.EmissionEpochLength){
    val multiptlyIterQty: Int =
      if (height % constants.EmissionEpochLength == 0)
        Math.round(height / constants.EmissionEpochLength) +  1
      else Math.round(height / constants.EmissionEpochLength)
    (constants.InitialEmissionAmount * Math.pow(1 - constants.EmissionDecay, multiptlyIterQty)).toLong
  } else constants.InitialEmissionAmount
}