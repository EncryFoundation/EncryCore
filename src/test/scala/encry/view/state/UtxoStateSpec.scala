package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.account.Address
import encry.local.TestHelper
import encry.modifiers.mempool.PaymentTransaction
import encry.utils.FileHelper
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

class UtxoStateSpec extends PropSpec with Matchers {

  def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore = new LSMStore(dir, keySize = 32, keepVersions = 10)
    val indexStore = new LSMStore(dir, keySize = 32, keepVersions = 10)

    new UtxoState(EncryState.genesisStateVersion, stateStore, indexStore, None) {
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

    val validTxs = keys.zip(bxs).map { case (pk, bx) =>
      val proposition = pk.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(bx).map(_.id)
      val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, factory.Props.boxValue - 100))
      val sig = PrivateKey25519Companion.sign(
        pk,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val invalidTxs = keys.map { pk =>
      val proposition = pk.publicImage
      val fee = factory.Props.txFee
      val timestamp = 123456789L
      val useBoxes =
        IndexedSeq(factory.genAssetBox(Address @@ "3goCpFrrBakKJwxk7d4oY5HN54dYMQZbmVWKvQBPZPDvbL3hHp")).map(_.id)
      val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, 30000L))
      val sig = PrivateKey25519Companion.sign(
        pk,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val filteredValidTxs = state.filterValid(validTxs)

    filteredValidTxs.size shouldEqual validTxs.size

    val filteredInvalidTxs = state.filterValid(invalidTxs)

    filteredInvalidTxs.isEmpty shouldBe true

    val filteredValidAndInvalidTxs = state.filterValid(validTxs ++ invalidTxs)

    filteredValidAndInvalidTxs.size shouldEqual validTxs.size
  }
}
