package encry.consensus

import encry.settings.Constants
import encry.view.history.History.Height
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable

class PowLinearControllerSpec extends PropSpec with Matchers {

  property("getHeightsForRetargetingAt()") {

    val retargetingAtHeight: Int = 1001

    val expected: immutable.Seq[Int] = (0 to Constants.Chain.RetargetingEpochsQty).reverse
      .map(i => (retargetingAtHeight - 1) - i * Constants.Chain.EpochLength).filter(_ >= Constants.Chain.GenesisHeight)

    val heights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ retargetingAtHeight)

    heights sameElements expected shouldBe true
  }
}
