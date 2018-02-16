package encry.settings

object Constants {

  val hashLength: Int = 32
  val MaxTarget: BigInt = BigInt(1, Array.fill(hashLength)((-1).toByte))

//  val InitialDifficulty: Difficulty = BigInt(1)
//  val InitialNBits: Long = RequiredDifficulty.encodeCompactBits(InitialDifficulty)

  val ModifierIdSize: Int = hashLength

  val keepVersions: Int = 200

  val feeMinAmount: Int = 2

  val txByteCost: Float = 0.00046f

  object Chain {

    val coinbaseHeightLock = 1

    val genesisBoxesQty = 100

    val genesisBoxesAmount = 20L

    val initialEmissionAmount = 2000L

    val deflationInterval = 20

    val deflationFactor = 0.8
  }

  // TODO: Move to node settings.
  object Store {

    val stateKeepVersions: Int = 100

    val indexKeepVersions: Int = 100

    val walletKeepVersions: Int = 100

    val stateStoreKeyLength: Int = 32

    val indexStoreKeyLength: Int = 32
  }
}
