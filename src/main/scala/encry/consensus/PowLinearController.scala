package encry.consensus

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.Constants
import encry.view.history.Height

object PowLinearController {

  private val chainParams = Constants.Chain

  def getDifficulty(previousHeaders: Seq[(Int, EncryBlockHeader)]): NBits = {
    if (previousHeaders.length == chainParams.RetargetingEpochsQty) {
      val data: Seq[(Int, Difficulty)] = previousHeaders.sliding(2).toList.map { d =>
        val start = d.head
        val end = d.last
        require(end._1 - start._1 == chainParams.EpochLength, s"Incorrect heights interval for $d")
        val diff = Difficulty @@ (end._2.requiredDifficulty * chainParams.DesiredBlockInterval.toMillis *
          chainParams.EpochLength / (end._2.timestamp - start._2.timestamp))
        (end._1, diff)
      }
      val diff = interpolate(data)
      if (diff >= 1) diff else chainParams.InitialNBits
    } else previousHeaders.maxBy(_._1)._2.nBits
  }

  // y = a + bx
  private[consensus] def interpolate(data: Seq[(Int, Difficulty)]): NBits = {
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

    val point = data.map(_._1).max + chainParams.EpochLength
    DifficultySerializer.encodeCompactBits(b + k * point / PrecisionConstant)
  }

  // Used to provide `getTimedelta()` with the sequence of headers of right heights.
  def getHeightsForRetargetingAt(height: Height): Seq[Height] = {
    if ((height - 1) > chainParams.RetargetingEpochsQty)
      (0 until chainParams.RetargetingEpochsQty)
        .map(i => (height - 1) - i).reverse.map(i => Height @@ i)
    else
      (0 until height)
        .map(i => (height - 1) - i).filter(i => i > 1).reverse.map(i => Height @@ i)
  }

  val PrecisionConstant: Int = 1000000000
}
