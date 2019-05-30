package encry.consensus

import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height > TestNetConstants.EmissionEpochLength){
    val multiptlyIterQty: Int =
      if (height % TestNetConstants.EmissionEpochLength == 0)
        Math.round(height / TestNetConstants.EmissionEpochLength) +  1
      else Math.round(height / TestNetConstants.EmissionEpochLength)
    (TestNetConstants.InitialEmissionAmount * Math.pow(1 - TestNetConstants.EmissionDecay, multiptlyIterQty)).toLong
  } else TestNetConstants.InitialEmissionAmount
}