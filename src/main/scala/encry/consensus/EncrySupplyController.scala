package encry.consensus

import encry.modifiers.state.box.Box.Amount
import encry.settings.Constants
import encry.view.history.History.Height

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height >= Constants.Chain.DeflationInterval) {
    (scala.math.pow(Constants.Chain.DeflationFactor, (height / Constants.Chain.DeflationInterval).floor) *
      Constants.Chain.InitialEmissionAmount).toLong
  } else Constants.Chain.InitialEmissionAmount
}
