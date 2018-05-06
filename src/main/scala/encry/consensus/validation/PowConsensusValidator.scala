package encry.consensus.validation

import encry.consensus.{Difficulty, DifficultySerializer, NBits, PowLinearController}

object PowConsensusValidator {

  // Should be used after the difficulty validation.
  def validatePow(headerHash: Array[Byte], difficulty: NBits): Boolean = {
    assert(difficulty > 0, "Difficulty coefficient can not be less than 1")
    val target = PowLinearController.getTarget(Difficulty @@ DifficultySerializer.decodeCompactBits(difficulty))
    BigInt(1, headerHash) < target
  }
}
