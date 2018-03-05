package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.account.Address
import encry.local.TestHelper
import encry.modifiers.mempool.TransactionFactory
import encry.settings.Constants
import encry.utils.FileHelper
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

class UtxoStateSpec extends PropSpec with Matchers {

  def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore = new LSMStore(dir, keySize = 32, keepVersions = 10)

    new UtxoState(EncryState.genesisStateVersion, Constants.Chain.genesisHeight, stateStore, None) {
      override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
        PersistentBatchAVLProver.create(
          p, storage, paranoidChecks = true
        ).get
    }
  }

  property("FilterValid(txs) should return only valid txs (against current state).") {

    val bxs = TestHelper.genAssetBoxes

    val bh = BoxHolder(bxs)

    val state = utxoFromBoxHolder(bh, FileHelper.getRandomTempDir, None)

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val fee = factory.Props.txFee
    val timestamp = 1234567L

    val validTxs = keys.zip(bxs).map { case (k, bx) =>
      val useBoxes = IndexedSeq(bx)
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, factory.Props.recipientAddr, factory.Props.boxValue - 100)
    }

    val invalidTxs = keys.map { k =>
      val useBoxes =
        IndexedSeq(factory.genAssetBox(Address @@ "4iGUxZy1uy9m1xsEL26VuUZ8Q23W961PPvDTPW3t2jXuCJCPuy"))
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, factory.Props.recipientAddr, 3000000L)
    }

    val filteredValidTxs = state.filterValid(validTxs)

    filteredValidTxs.size shouldEqual validTxs.size

    val filteredInvalidTxs = state.filterValid(invalidTxs)

    filteredInvalidTxs.isEmpty shouldBe true

    val filteredValidAndInvalidTxs = state.filterValid(validTxs ++ invalidTxs)

    filteredValidAndInvalidTxs.size shouldEqual validTxs.size
  }
}
