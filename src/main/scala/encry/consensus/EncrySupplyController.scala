package encry.consensus

import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.Constants
import encry.view.history.Height

object EncrySupplyController {

  def supplyAt(height: Height): Amount = if (height >= Constants.Chain.DeflationInterval) {
    (scala.math.pow(Constants.Chain.DeflationFactor, (height / Constants.Chain.DeflationInterval).floor) *
      Constants.Chain.InitialEmissionAmount).toLong
  } else Constants.Chain.InitialEmissionAmount

  def supplyBoxAt(height: Height): AssetBox =
    AssetBox(EncryProposition.heightLocked(height), height * 9, supplyAt(height))

  def totalSupplyBoxes: IndexedSeq[AssetBox] =
    (Constants.Chain.PreGenesisHeight until Constants.Chain.EmissionEpochLength).map(i => supplyBoxAt(Height @@ i))
}
