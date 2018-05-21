package encry.view.state

import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.{EncryBoxStateChanges, Insertion, Removal}
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

class EncryStateSpec extends PropSpec with Matchers with EncryGenerator {

  property("extractStateChanges() should support instant output spending") {

    val spendingTxs: Seq[EncryTransaction] = genChainSpendingTxs(1000)

    val state: UtxoState = genUtxoState

    val stateOperations: EncryBoxStateChanges = state.extractStateChanges(spendingTxs)

    stateOperations.operations.count(_.isInstanceOf[Removal]) == 1 shouldBe true    // Box to be spent by the first generated transaction.
    stateOperations.operations.count(_.isInstanceOf[Insertion]) == 2 shouldBe true  // 2 Boxes created by the last transaction.
  }

  property("EncryState.genesisBoxes() output equality") {

    val g1 = EncryState.genesisBoxes
    val g2 = EncryState.genesisBoxes
    val g3 = EncryState.genesisBoxes

    g1.zip(g2).zip(g3).forall { case ((e1, e2), e3) =>
      (e1.bytes sameElements e2.bytes) && (e2.bytes sameElements e3.bytes)
    } shouldBe true
  }
}
