package encry.consensus.validation

object PowConsensusValidator {

  def validatePow(headerHash: Array[Byte], difficulty: BigInt): Boolean = {
    assert(difficulty > 0, "Difficulty coefficient can not be less than 1")
    val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
    val target = maxTarget / difficulty
    println(s"Current target is: $target")
    BigInt(1, headerHash) < target
  }
}
