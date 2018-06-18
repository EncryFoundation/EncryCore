package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.account.Address
import encry.crypto.PrivateKey25519
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.settings.{Algos, Constants}
import encry.utils.{EncryGenerator, FileHelper, TestHelper}
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random

class UtxoStateSpec extends PropSpec with Matchers with EncryGenerator {

  def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore = new LSMStore(dir, keySize = 32, keepVersions = 10)

    new UtxoState(EncryState.genesisStateVersion, Constants.Chain.GenesisHeight, stateStore, 0L, None) {
      override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Algos.HF] =
        PersistentBatchAVLProver.create(
          p, storage, paranoidChecks = true
        ).get
    }
  }

  property("Proofs for transaction") {

    val (privKey: PrivateKey, pubKey: PublicKey) = Curve25519.createKeyPair(Random.randomBytes())
    val secret: PrivateKey25519 = PrivateKey25519(privKey, pubKey)

    val initialBoxes: Seq[AssetBox] = genValidAssetBoxes(secret, amount = 100000, qty = 50)

    val bh: BoxHolder = BoxHolder(initialBoxes)

    val state: UtxoState = utxoFromBoxHolder(bh, FileHelper.getRandomTempDir, None)

    val regularTransactions: Seq[EncryTransaction] = initialBoxes.map { bx =>
      TransactionFactory.defaultPaymentTransactionScratch(
        secret, 10000, timestamp, IndexedSeq(bx), randomAddress, 5000)
    }

    val fees: Amount = regularTransactions.map(_.fee).sum

    val coinbase: EncryTransaction = TransactionFactory.coinbaseTransactionScratch(secret.publicImage, timestamp, IndexedSeq.empty, fees)

    val transactions: Seq[EncryTransaction] = regularTransactions.sortBy(_.timestamp) :+ coinbase

    val (_: SerializedAdProof, adDigest: ADDigest) = state.generateProofs(transactions).get

    state.applyBlockTransactions(transactions, adDigest).isSuccess shouldBe true
  }

  property("FilterValid(txs) should return only valid txs (against current state).") {

    val bxs = TestHelper.genAssetBoxes

    val bh = BoxHolder(bxs)

    val state = utxoFromBoxHolder(bh, FileHelper.getRandomTempDir, None)

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val fee = factory.Props.txFee

    val validTxs = keys.zip(bxs).map { case (k, bx) =>
      val useBoxes = IndexedSeq(bx)
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, factory.Props.recipientAddr, factory.Props.boxValue - 4300)
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
