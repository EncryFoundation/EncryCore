package encry.consensus

import encry.settings.ConsensusSettings

import scala.concurrent.duration.FiniteDuration

object PowLinearController {

  def getTarget(difficulty: Difficulty): BigInt =
    ConsensusSettings.maxTarget / difficulty

  // Retargeting to adjust difficulty.
  def getNewTarget(oldTarget: BigInt, lastNBlocksEmissionIntervalSec: FiniteDuration): BigInt =
    oldTarget * lastNBlocksEmissionIntervalSec.toMillis / ConsensusSettings.desiredBlockEmissionInterval.toMillis

  def getNewDifficulty(oldTarget: BigInt, lastNBlocksEmissionIntervalMins: FiniteDuration): Difficulty =
    Difficulty @@ (ConsensusSettings.maxTarget / getNewTarget(oldTarget, lastNBlocksEmissionIntervalMins))
}
