package encry.consensus

import encry.settings.ConsensusSettings

object PowConsensus {

  def getTarget(difficulty: BigInt): BigInt = ConsensusSettings.maxTarget / difficulty

  // Retargeting to adjust difficulty
  def getNewTarget(oldTarget: BigInt, lastNBlocksEmissionPeriodMins: Int): BigInt =
    oldTarget * (lastNBlocksEmissionPeriodMins / ConsensusSettings.difficultyRetargetingIntervalMins)

  def getNewDifficulty(oldTarget: BigInt, lastNBlocksEmissionPeriodMins: Int): BigInt =
    ConsensusSettings.maxTarget / getNewTarget(oldTarget, lastNBlocksEmissionPeriodMins)
}
