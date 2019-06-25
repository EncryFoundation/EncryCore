package encry.view.state

import encry.utils.EncryGenerator
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.scalatest.{Matchers, PropSpec}

class EncryStateSpec extends PropSpec with Matchers with EncryGenerator {

  property("EncryState.genesisBoxes() output equality") {

    val a = Seq(0,1,3,4,5)

    println(a.dropWhile(_ != 1))

    val g1: Seq[AssetBox] = UtxoStateWithoutAVL.initialStateBoxes
    val g2: Seq[AssetBox] = UtxoStateWithoutAVL.initialStateBoxes
    val g3: Seq[AssetBox] = UtxoStateWithoutAVL.initialStateBoxes

    g1.zip(g2).zip(g3).forall { case ((e1, e2), e3) =>
      (e1.bytes sameElements e2.bytes) && (e2.bytes sameElements e3.bytes)
    } shouldBe true
  }
}
