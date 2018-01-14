package encry.consensus.validation

import encry.consensus.{Difficulty, PowLinearController}

object PowConsensusValidator {

  // Should be used after the difficulty validation.
  def validatePow(headerHash: Array[Byte], difficulty: Difficulty): Boolean = {
    assert(difficulty > 0, "Difficulty coefficient can not be less than 1")
    val target = PowLinearController.getTarget(difficulty)
    BigInt(1, headerHash) < target
  }
}
