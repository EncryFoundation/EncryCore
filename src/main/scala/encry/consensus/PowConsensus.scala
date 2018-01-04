package encry.consensus

import encry.consensus.validation.PowConsensusValidator
import encry.settings.ChainSettings

class PowConsensus(chainSettings: ChainSettings) {

  val difficultyController: PowLinearController = new PowLinearController(chainSettings)
  val validator: PowConsensusValidator.type = PowConsensusValidator
}
