package encry.view.state

import org.scalatest.FunSuite

class EncryStateTest extends FunSuite {

  test("EncryState.genesisBoxes() output equality") {

    val g1 = EncryState.genesisBoxes
    val g2 = EncryState.genesisBoxes
    val g3 = EncryState.genesisBoxes

    assert(g1.zip(g2).zip(g3).forall { case ((e1, e2), e3) =>
      (e1.bytes sameElements e2.bytes) && (e2.bytes sameElements e3.bytes) })
  }
}
