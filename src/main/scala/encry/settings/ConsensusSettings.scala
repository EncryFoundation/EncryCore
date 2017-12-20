package encry.settings

object ConsensusSettings {
  val initialDifficulty: BigInt = 15000
  lazy val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  val difficultyRetargetingIntervalBlocks: Int = 2016
  val blockEmissionIntervalMins: Int = 1
  lazy val difficultyRetargetingIntervalMins: Int = difficultyRetargetingIntervalMins * blockEmissionIntervalMins
}
