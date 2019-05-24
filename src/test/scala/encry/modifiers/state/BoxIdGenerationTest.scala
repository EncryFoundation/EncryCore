package encry.modifiers.state

import encry.modifiers.InstanceFactory
import encry.modifiers.history.ADProofsFunctions
import org.encryfoundation.common.modifiers.history.ADProofs
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.{EncryBoxStateChanges, Insertion, Removal}
import org.scalatest.FunSuite

class BoxIdGenerationTest extends FunSuite with InstanceFactory {

  // Extracts `state changes` from the given sequence of transactions.
  private def getAllStateChanges(txs: Seq[Transaction]): EncryBoxStateChanges = {
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx.inputs.map(u => Removal(u.boxId)) ++ tx.newBoxes.map(bx => Insertion(bx))
      }
    )
  }

  test("Unique box id generation") {
    val transactions = (0 to 5).map(_ => paymentTransactionDynamic)

    val mods = getAllStateChanges(transactions).operations.map(ADProofsFunctions.toModification)

    assert(mods.foldLeft(true) { case (b, m) =>
      b && mods.filter(_.key sameElements m.key).size == 1
    })
  }
}
