package encry.network

import org.scalatest.{Matchers, PropSpec}

import scala.util.Random

class ModifiersHolderTest extends PropSpec with Matchers {

  property("Correct counting of gaps") {

    val firstMinRange: Int = Random.nextInt(1000) + 1

    val firstMaxRange: Int = firstMinRange + Random.nextInt(1000)

    val secondMinRange: Int = firstMaxRange + Random.nextInt(1000)

    val secondMaxRange: Int = secondMinRange + Random.nextInt(10000)

    val thirdMinRange: Int = secondMaxRange + Random.nextInt(1000)

    val thirdMaxRange: Int = thirdMinRange + Random.nextInt(1000)

    val fakeHeightChain: Seq[Int] = (firstMinRange to firstMaxRange) ++ (secondMinRange to secondMaxRange) ++ (thirdMinRange to thirdMaxRange)

    ModifiersHolder.countGaps(fakeHeightChain) shouldEqual
      Seq((1, firstMinRange - 1), (firstMaxRange + 1, secondMinRange - 1), (secondMaxRange + 1, thirdMinRange - 1))
  }
}
