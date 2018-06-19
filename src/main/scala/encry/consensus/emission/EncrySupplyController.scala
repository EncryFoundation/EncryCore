package encry.consensus.emission

import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.Constants
import encry.view.history.Height
import scorex.core.transaction.box.Box.Amount

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height >= Constants.Chain.DeflationInterval) {
    (scala.math.pow(Constants.Chain.DeflationFactor, (height / Constants.Chain.DeflationInterval).floor) *
      Constants.Chain.InitialEmissionAmount).toLong
  } else Constants.Chain.InitialEmissionAmount

  def supplyBoxAt(height: Height): AssetBox =
    AssetBox(EncryProposition.heightLocked(height), height * 9, EncrySupplyController.supplyAt(height))

  def totalSupplyBoxes: Seq[AssetBox] =
    (Constants.Chain.PreGenesisHeight until Constants.Chain.EmissionEpochLength).map(i => supplyBoxAt(Height @@ i))
}
