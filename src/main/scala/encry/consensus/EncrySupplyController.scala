package encry.consensus

import encry.settings.TestConstants
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height > TestConstants.EmissionEpochLength){
    val multiptlyIterQty: Int =
      if (height % TestConstants.EmissionEpochLength == 0)
        Math.round(height / TestConstants.EmissionEpochLength) +  1
      else Math.round(height / TestConstants.EmissionEpochLength)
    (TestConstants.InitialEmissionAmount * Math.pow(1 - TestConstants.EmissionDecay, multiptlyIterQty)).toLong
  } else TestConstants.InitialEmissionAmount
}