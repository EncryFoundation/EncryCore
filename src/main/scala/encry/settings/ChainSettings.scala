package encry.settings

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

import encry.view.history.Height

case class ChainSettings(initialDifficulty: BigInt,
                         desiredBlockInterval: FiniteDuration,
                         retargetingEpochsQty: Int,
                         epochLength: Int,
                         blockMaxSize: Int) {

  lazy val maxRollback: Long = 600.days.toMillis / desiredBlockInterval.toMillis
}

object ChainSettings {
  val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))  // 2^256
  val genesisHeight: Height = Height @@ 0
}