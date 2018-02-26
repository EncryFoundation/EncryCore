package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.account.{Account, Address}
import encry.crypto.{PublicKey25519, Signature25519}
import encry.local.TestHelper
import encry.modifiers.mempool.PaymentTransaction
import encry.modifiers.mempool.directive.TransferDirective
import encry.utils.FileHelper
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import scorex.crypto.signatures.Curve25519

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
      val proposition = PublicKey25519(pk.publicKeyBytes)
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(bx).map(_.id)
      val outputs = IndexedSeq(TransferDirective(factory.Props.recipientAddr, factory.Props.boxValue - 100, 1))
      val sig = Signature25519(Curve25519.sign(
        pk.privKeyBytes,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      ))
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val invalidTxs = keys.map { pk =>
      val proposition = PublicKey25519(pk.publicKeyBytes)
      val fee = factory.Props.txFee
      val timestamp = 123456789L
      val useBoxes =
        IndexedSeq(factory.genAssetBox(Address @@ "4iGUxZy1uy9m1xsEL26VuUZ8Q23W961PPvDTPW3t2jXuCJCPuy")).map(_.id)
      val outputs = IndexedSeq(TransferDirective(factory.Props.recipientAddr, 30000L, 1))
      val sig = Signature25519(Curve25519.sign(
        pk.privKeyBytes,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      ))
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
