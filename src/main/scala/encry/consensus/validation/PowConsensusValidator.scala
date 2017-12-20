package encry.consensus.validation

import encry.consensus.PowConsensus

object PowConsensusValidator {

  // Should be used after the difficulty validation.
  def validatePow(headerHash: Array[Byte], difficulty: BigInt): Boolean = {
    assert(difficulty > 0, "Difficulty coefficient can not be less than 1")
    val target = PowConsensus.getTarget(difficulty)
    println(s"Current target is: $target")
    BigInt(1, headerHash) < target
  }

  // TODO: Implement.
  def validateDifficulty(actualDifficulty: BigInt): Boolean = true
}
