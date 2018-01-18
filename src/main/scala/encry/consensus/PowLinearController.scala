package encry.consensus

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.ChainSettings
import encry.view.history.Height
import java.util.concurrent.TimeUnit.MILLISECONDS

import scala.concurrent.duration.FiniteDuration

class PowLinearController(chainSettings: ChainSettings) {

  import PowLinearController._

  def getDifficulty(previousHeaders: Seq[(Int, EncryBlockHeader)]): Difficulty = {
    if (previousHeaders.size == chainSettings.retargetingEpochsQty) {
      val data: Seq[(Int, Difficulty)] = previousHeaders.sliding(2).toList.map { d =>
        val start = d.head
        val end = d.last
        require(end._1 - start._1 == chainSettings.epochLength, s"Incorrect heights interval for $d")
        val diff = Difficulty @@ (end._2.difficulty * chainSettings.desiredBlockInterval.toMillis *
          chainSettings.epochLength / (end._2.timestamp - start._2.timestamp))
        (end._1, diff)
      }
      val diff = interpolate(data)
      if (diff >= 1) diff else Difficulty @@ chainSettings.initialDifficulty
    } else previousHeaders.maxBy(_._1)._2.difficulty
  }

  // y = a + bx
  private[consensus] def interpolate(data: Seq[(Int, Difficulty)]): Difficulty = {
    val size = data.size
    val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
    val x: Iterable[BigInt] = data.map(d => BigInt(d._1))
    val x2: Iterable[BigInt] = data.map(d => BigInt(d._1) * d._1)
    val y: Iterable[BigInt] = data.map(d => d._2)
    val xySum = xy.sum
    val x2Sum = x2.sum
    val ySum = y.sum
    val xSum = x.sum

    val k: BigInt = (xySum * size - xSum * ySum) * PrecisionConstant / (x2Sum * size - xSum * xSum)
    val b: BigInt = (ySum * PrecisionConstant - k * xSum) / size / PrecisionConstant

    val point = data.map(_._1).max + chainSettings.epochLength
    Difficulty @@ (b + k * point / PrecisionConstant)
  }

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

  val PrecisionConstant: Int = 1000000000

  def getTarget(difficulty: Difficulty): BigInt =
    ChainSettings.maxTarget / difficulty
}
