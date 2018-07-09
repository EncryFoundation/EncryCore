package encry.utils

object Chain {

  def countGaps(blocksHeight: Seq[Int]): Seq[(Int, Int)] = blocksHeight.foldLeft(Seq[(Int, Int)](), 0) {
    case ((gaps, prevHeight), blockHeight) =>
      if (prevHeight + 1 != blockHeight) (gaps :+ (prevHeight + 1, blockHeight - 1), blockHeight)
      else (gaps, blockHeight)
  }._1
}
