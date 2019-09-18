package encry.consensus

import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height

object EncrySupplyController {

  def supplyAt(height: Height, initialEmissionAmount: Int, emissionEpochLength: Int, emissionDecay: Double): Amount =
    if (height > emissionEpochLength) {
      val multiptlyIterQty: Int =
        if (height % emissionEpochLength == 0)
          Math.round(height / emissionEpochLength) + 1
        else Math.round(height / emissionEpochLength)
      (initialEmissionAmount * Math.pow(1 - emissionDecay, multiptlyIterQty)).toLong
    } else initialEmissionAmount
}