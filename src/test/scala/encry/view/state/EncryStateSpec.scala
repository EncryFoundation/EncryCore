package encry.view.state

import encry.modifiers.state.box.AssetBox
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

class EncryStateSpec extends PropSpec with Matchers with EncryGenerator {

  property("EncryState.genesisBoxes() output equality") {

    val g1: Seq[AssetBox] = EncryState.totalSupplyBoxes
    val g2: Seq[AssetBox] = EncryState.totalSupplyBoxes
    val g3: Seq[AssetBox] = EncryState.totalSupplyBoxes

    g1.zip(g2).zip(g3).forall { case ((e1, e2), e3) =>
      (e1.bytes sameElements e2.bytes) && (e2.bytes sameElements e3.bytes)
    } shouldBe true
  }
}
