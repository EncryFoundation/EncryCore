package encry.consensus

import encry.settings.ConstantsSettings
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable

class PowLinearControllerSpec extends PropSpec with Matchers with ConstantsSettings {

  property("getHeightsForRetargetingAt()") {

    val retargetingAtHeight: Int = 1001

    val expected: immutable.Seq[Int] = (0 to constants.RetargetingEpochsQty).reverse
      .map(i => (retargetingAtHeight - 1) - i * constants.EpochLength).filter(_ >= constants.GenesisHeight)

    val heights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ retargetingAtHeight,
      constants.EpochLength, constants.RetargetingEpochsQty)

    heights sameElements expected shouldBe true
  }
}
