package encry.consensus.emission

import encry.consensus.EncrySupplyController
import encry.settings.Settings
import encry.utils.EncryGenerator
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._

class EncrySupplyControllerTest extends PropSpec with Matchers with EncryGenerator with Settings {

  val epochLen = 10

  val blocksPerHour: Int =  ((60 * 60).seconds / settings.constants.DesiredBlockInterval).toInt

  val blocksPerYear: Int = blocksPerHour * 24 * 365

  property("testSupplyAt") {

    val epochSupply = (0 until blocksPerYear * epochLen).map(h => EncrySupplyController.supplyAt(Height @@ h,
      settings.constants))

    val atEndEpochSupply = epochSupply.last

    val epochSupplyTotal = epochSupply.sum

    val finalHeight = epochSupply.zipWithIndex.find(i => i._1 == 0).map(h => h._2).getOrElse(-1)

    val firstYearSupply = epochSupply.zipWithIndex.filter(i => i._2 <= blocksPerYear).map(_._1).sum

    atEndEpochSupply < settings.constants.InitialEmissionAmount shouldBe true
  }

}
