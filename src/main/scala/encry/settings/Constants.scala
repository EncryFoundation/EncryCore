package encry.settings

import encry.consensus.Difficulty
import encry.view.history.Height

import scala.concurrent.duration._

object Constants {

  val digestLength: Int = 32

  val ModifierIdSize: Int = digestLength

  val keepVersions: Int = 200

  val feeMinAmount: Int = 2

  val txByteCost: Float = 0.00046f

  object Chain {

    val coinbaseHeightLock = 1

    val genesisBoxesQty = 100

    val genesisBoxesAmount = 1L

    // Number of fractions in one Encry Token.
    val fractionsPerToken = 1000000

    val initialEmissionAmount: Int = 2 * fractionsPerToken

    val deflationInterval = 100

    val deflationFactor = 0.9998

    // Desired time interval between blocks
    val desiredBlockInterval: FiniteDuration = 30.seconds

    val initialDifficulty: Difficulty = Difficulty @@ BigInt(100000)

    // Number of last epochs that will  be used for difficulty recalculation
    val retargetingEpochsQty = 30

    val epochLength = 1

    val blockMaxSize = 10000

    val maxTarget = BigInt(1, Array.fill(digestLength)((-1).toByte))

    val genesisHeight: Height = Height @@ 1

    val maxRollback: Long = 600.days.toMillis / desiredBlockInterval.toMillis

    val maxTimeDrift: Long = 10 * desiredBlockInterval.toMillis
  }
}
