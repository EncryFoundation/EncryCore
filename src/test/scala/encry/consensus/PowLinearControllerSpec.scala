package encry.consensus

import encry.settings.Settings
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable

class PowLinearControllerSpec extends PropSpec with Matchers with Settings {

  property("getHeightsForRetargetingAt()") {

    val retargetingAtHeight: Int = 1001

    val expected: immutable.Seq[Int] = (0 to settings.constants.RetargetingEpochsQty).reverse
      .map(i => (retargetingAtHeight - 1) - i * settings.constants.EpochLength).filter(_ >= settings.constants.GenesisHeight)

    val heights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ retargetingAtHeight,
      settings.constants.EpochLength, settings.constants.RetargetingEpochsQty)

    heights sameElements expected shouldBe true
  }
}
