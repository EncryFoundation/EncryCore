package encry.settings

import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit.SECONDS

object ConsensusSettings {

  val initialDifficulty: BigInt = 15000
  val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  val desiredEpochIntervalSec: FiniteDuration = FiniteDuration(60, SECONDS)
  val retargetingEpochsQty: Int = 10
}
