package encry.modifiers.mempool

import encry.utils.Utils.randomAddress
import encry.utils.TestEntityGenerator._
import encry.utils.TestHelper
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.scalatest.{Matchers, PropSpec}

class TransactionSpec extends PropSpec with Matchers {

  private val txValid = paymentTransactionValid

  property("semanticValidity of valid tx") {

    txValid.semanticValidity.isSuccess shouldBe true
  }

  property("semanticValidity of invalid tx (Inputs duplication)") {

    val tx: Transaction = {
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(TestHelper.genAssetBox(publicKey.address.address))

      TransactionFactory.defaultPaymentTransactionScratch(privKey, 100, timestamp, useBoxes ++ useBoxes ++ useBoxes,
        randomAddress, TestHelper.Props.txAmount)
    }

    tx.semanticValidity.isSuccess shouldBe false
  }

  property("semanticValidity of invalid tx (negative fee)") {

    val tx: Transaction = {
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(TestHelper.genAssetBox(publicKey.address.address))

      TransactionFactory.defaultPaymentTransactionScratch(privKey, -100, timestamp, useBoxes,
        randomAddress, TestHelper.Props.txAmount)
    }

    tx.semanticValidity.isSuccess shouldBe false
  }

  property("semanticValidity of invalid tx (Empty inputs)") {

    val tx: Transaction = {
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty

      TransactionFactory.defaultPaymentTransactionScratch(privKey, -100, timestamp, useBoxes,
        randomAddress, TestHelper.Props.txAmount)
    }

    tx.semanticValidity.isSuccess shouldBe false
  }

  property("semanticValidity of invalid tx (Too many inputs)") {

    val tx: Transaction = {
      val useBoxes: IndexedSeq[AssetBox] = (0 to Short.MaxValue + 10)
        .foldLeft(IndexedSeq.empty[AssetBox]) { case (acc, _) => acc :+ TestHelper.genAssetBox(publicKey.address.address) }

      TransactionFactory.defaultPaymentTransactionScratch(privKey, 100, timestamp, useBoxes,
        randomAddress, TestHelper.Props.txAmount)
    }

    tx.semanticValidity.isSuccess shouldBe false
  }
}
