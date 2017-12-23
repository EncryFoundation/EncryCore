package encry.settings

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

import encry.view.history.Height

case class ConsensusSettings(initialDifficulty: BigInt,
                             desiredEpochIntervalSec: FiniteDuration,
                             retargetingEpochsQty: Int) {

  lazy val maxRollback: Long = 600.days.toMillis / desiredEpochIntervalSec.toMillis
}

object ConsensusSettings {
  val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))  // 2^256
  val genesisHeight: Height = Height @@ 1
}