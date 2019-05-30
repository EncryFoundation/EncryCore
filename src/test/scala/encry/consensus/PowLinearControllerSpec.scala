package encry.consensus

import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable

class PowLinearControllerSpec extends PropSpec with Matchers {

  property("getHeightsForRetargetingAt()") {

    val retargetingAtHeight: Int = 1001

    val expected: immutable.Seq[Int] = (0 to TestNetConstants.RetargetingEpochsQty).reverse
      .map(i => (retargetingAtHeight - 1) - i * TestNetConstants.EpochLength).filter(_ >= TestNetConstants.GenesisHeight)

    val heights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ retargetingAtHeight)

    heights sameElements expected shouldBe true
  }
}
