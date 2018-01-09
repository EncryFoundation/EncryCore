package encry.consensus

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.ChainSettings
import encry.view.history.Height
import java.util.concurrent.TimeUnit.MILLISECONDS

import scala.concurrent.duration.FiniteDuration

class PowLinearController(chainSettings: ChainSettings) {

  import PowLinearController._

  // Retargeting to adjust difficulty.
  def getNewTarget(oldTarget: BigInt, lastEpochsIntervalMs: FiniteDuration): BigInt =
    oldTarget * lastEpochsIntervalMs.toMillis / (chainSettings.desiredBlockInterval.toMillis *
      chainSettings.retargetingEpochsQty)

  def getNewDifficulty(oldDifficulty: Difficulty, lastEpochsIntervalMs: FiniteDuration): Difficulty =
    Difficulty @@ (ChainSettings.maxTarget / getNewTarget(getTarget(oldDifficulty), lastEpochsIntervalMs))

  // Used to provide `getTimedelta()` with the sequence of headers of right heights.
  def getHeightsForRetargetingAt(height: Height): Seq[Height] = {
    if ((height - 1) > chainSettings.retargetingEpochsQty)
      (0 until chainSettings.retargetingEpochsQty)
        .map(i => (height - 1) - i).reverse.map(i => Height @@ i)
    else
      (0 until height)
        .map(i => (height - 1) - i).filter(i => i > 1).reverse.map(i => Height @@ i)
  }

  def getTimedelta(headers: Seq[EncryBlockHeader]): FiniteDuration = {
    val start = headers.head.timestamp
    val end = headers.last.timestamp
    FiniteDuration(end - start, MILLISECONDS)
  }
}

object PowLinearController {

  def getTarget(difficulty: Difficulty): BigInt =
    ChainSettings.maxTarget / difficulty
}
