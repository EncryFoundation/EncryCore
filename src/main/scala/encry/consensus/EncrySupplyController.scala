package encry.consensus

import encry.settings.Constants
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height > Constants.Chain.EmissionEpochLength){
    val multiptlyIterQty: Int =
      if (height % Constants.Chain.EmissionEpochLength == 0)
        Math.round(height / Constants.Chain.EmissionEpochLength) +  1
      else Math.round(height / Constants.Chain.EmissionEpochLength)
    (Constants.Chain.InitialEmissionAmount * Math.pow(1 - Constants.Chain.EmissionDecay, multiptlyIterQty)).toLong
  } else Constants.Chain.InitialEmissionAmount
}