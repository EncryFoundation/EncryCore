package encry.settings

import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit.SECONDS

import encry.view.history.Height

case class ConsensusSettings(initialDifficulty: BigInt,
                             desiredEpochIntervalSec: FiniteDuration,
                             retargetingEpochsQty: Int)

object ConsensusSettings {
  val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))  // 2^256
  val genesisHeight: Height = Height @@ 1
}