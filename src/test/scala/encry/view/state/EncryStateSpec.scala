package encry.view.state

import encry.modifiers.state.box.CoinbaseBox
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

class EncryStateSpec extends PropSpec with Matchers with EncryGenerator {

  property("EncryState.genesisBoxes() output equality") {

    val g1: IndexedSeq[CoinbaseBox] = EncryState.genesisBoxes
    val g2: IndexedSeq[CoinbaseBox] = EncryState.genesisBoxes
    val g3: IndexedSeq[CoinbaseBox] = EncryState.genesisBoxes

    g1.zip(g2).zip(g3).forall { case ((e1, e2), e3) =>
      (e1.bytes sameElements e2.bytes) && (e2.bytes sameElements e3.bytes)
    } shouldBe true
  }
}
