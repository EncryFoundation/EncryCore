package encry.modifiers.state

import encry.modifiers.InstanceFactory
import encry.modifiers.history.ADProofs
import encry.modifiers.mempool.BaseTransaction
import encry.modifiers.state.box.{EncryBoxStateChanges, Insertion, Removal}
import org.scalatest.FunSuite

class BoxIdGenerationTest extends FunSuite with InstanceFactory {

  // Extracts `state changes` from the given sequence of transactions.
  private def getAllStateChanges(txs: Seq[BaseTransaction]): EncryBoxStateChanges = {
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx.inputs.map(u => Removal(u.boxId)) ++ tx.newBoxes.map(bx => Insertion(bx))
      }
    )
  }

  test("Unique box id generation") {
    val transactions = (0 to 5).map(_ => paymentTransactionDynamic)

    val mods = getAllStateChanges(transactions).operations.map(ADProofs.toModification)

    assert(mods.foldLeft(true) { case (b, m) =>
      b && mods.filter(_.key sameElements m.key).size == 1
    })
  }
}
