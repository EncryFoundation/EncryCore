package encry.consensus.emission

import encry.settings.Constants
import encry.view.history.Height
import scorex.core.transaction.box.Box.Amount

object TokenSupplyController {

  def supplyAt(height: Height): Amount = if (height >= Constants.Chain.deflationInterval) {
    (scala.math.pow(Constants.Chain.deflationFactor, (height / Constants.Chain.deflationInterval).floor) *
      Constants.Chain.initialEmissionAmount).toLong
  } else Constants.Chain.initialEmissionAmount
}
