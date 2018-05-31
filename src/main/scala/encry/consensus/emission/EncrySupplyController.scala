package encry.consensus.emission

import encry.settings.Constants
import encry.view.history.Height
import scorex.core.transaction.box.Box.Amount

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height >= Constants.Chain.DeflationInterval) {
    (scala.math.pow(Constants.Chain.DeflationFactor, (height / Constants.Chain.DeflationInterval).floor) *
      Constants.Chain.InitialEmissionAmount).toLong
  } else Constants.Chain.InitialEmissionAmount
}
