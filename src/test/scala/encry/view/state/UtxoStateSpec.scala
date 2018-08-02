package encry.view.state

import java.io.File
import akka.actor.ActorRef
import encry.modifiers.mempool.{EncryTransaction, Pay2PubKeyAddress, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.Box.Amount
import encry.settings.Algos.HF
import encry.settings.{Algos, Constants}
import encry.utils.{EncryGenerator, FileHelper, TestHelper}
import encry.view.history.Height
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
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

    val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
      PersistentBatchAVLProver.create(p, storage).get
    }

    new UtxoState(persistentProver, EncryState.genesisStateVersion, Constants.Chain.GenesisHeight, stateStore, 0L, None)
  }

  property("Proofs for transaction") {

    val (privKey: PrivateKey, pubKey: PublicKey) = Curve25519.createKeyPair(Random.randomBytes())
    val secret: encry.crypto.PrivateKey25519 = encry.crypto.PrivateKey25519(privKey, pubKey)

    val initialBoxes: Seq[AssetBox] = genValidAssetBoxes(secret, amount = 100000, qty = 50)

    val bh: BoxHolder = BoxHolder(initialBoxes)

    val state: UtxoState = utxoFromBoxHolder(bh, FileHelper.getRandomTempDir, None)

    val regularTransactions: Seq[EncryTransaction] = initialBoxes.map { bx =>
      TransactionFactory.defaultPaymentTransactionScratch(
        secret, 10000, timestamp, IndexedSeq(bx), randomAddress, 5000)
    }

    val fees: Amount = regularTransactions.map(_.fee).sum

    val coinbase: EncryTransaction = TransactionFactory.coinbaseTransactionScratch(secret.publicImage, timestamp, 25L, fees, Height @@ 100)

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
        timestamp, useBoxes, randomAddress, factory.Props.boxValue - 4300)
    }

    val invalidTxs = keys.map { k =>
      val useBoxes =
        IndexedSeq(factory.genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address))
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, randomAddress, 3000000L)
    }

    val filteredValidTxs = state.filterValid(validTxs)

    filteredValidTxs.size shouldEqual validTxs.size

    val filteredInvalidTxs = state.filterValid(invalidTxs)

    filteredInvalidTxs.isEmpty shouldBe true

    val filteredValidAndInvalidTxs = state.filterValid(validTxs ++ invalidTxs)

    filteredValidAndInvalidTxs.size shouldEqual validTxs.size
  }

  property("Txs application") {

    val bxs = TestHelper.genAssetBoxes

    val bh = BoxHolder(bxs)

    val state = utxoFromBoxHolder(bh, FileHelper.getRandomTempDir, None)

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val fee = factory.Props.txFee

    val validTxs = keys.zip(bxs).map { case (k, bx) =>
      val useBoxes = IndexedSeq(bx)
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, randomAddress, factory.Props.boxValue - 4300)
    }

    val expectedDigest = state.generateProofs(validTxs)

    val applyTry = state.applyBlockTransactions(validTxs, expectedDigest.get._2)

    applyTry.isSuccess shouldBe true
  }
}
