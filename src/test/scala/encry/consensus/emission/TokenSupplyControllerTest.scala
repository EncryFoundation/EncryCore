package encry.consensus.emission

import encry.settings.Constants
import encry.view.history.Height
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._

class TokenSupplyControllerTest extends PropSpec with Matchers {

  val epochLen = 10

  val blocksPerHour: Int =  ((60 * 60).seconds / Constants.Chain.DesiredBlockInterval).toInt

  val blocksPerYear: Int = blocksPerHour * 24 * 365

  property("testSupplyAt") {

    val epochSupply = (0 until blocksPerYear * epochLen).map(h => TokenSupplyController.supplyAt(Height @@ h))

    val atEndEpochSupply = epochSupply.last

    val epochSupplyTotal = epochSupply.sum

    val finalHeight = epochSupply.zipWithIndex.find(i => i._1 == 0).map(h => h._2).getOrElse(-1)

    val firstYearSupply = epochSupply.zipWithIndex.filter(i => i._2 <= blocksPerYear).map(_._1).sum

    atEndEpochSupply < Constants.Chain.InitialEmissionAmount shouldBe true
  }

}
