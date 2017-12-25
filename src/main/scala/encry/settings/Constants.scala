package encry.settings

object Constants {
  val hashLength: Int = 32
  val MaxTarget: BigInt = BigInt(1, Array.fill(hashLength)((-1).toByte))
//  val InitialDifficulty: Difficulty = BigInt(1)
//  val InitialNBits: Long = RequiredDifficulty.encodeCompactBits(InitialDifficulty)
  val ModifierIdSize: Int = hashLength

  // TODO: Move to `NodeSettings`.
  val keepVersions: Int = 200
}
