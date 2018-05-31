package encry.utils

import java.io.File

import akka.actor.ActorRef
import encry.account.{Account, Address}
import encry.crypto.equihash.EquihashSolution
import encry.crypto.{PrivateKey25519, PublicKey25519}
import TestHelper.{Props, rndGen}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.state.box.proposition.AccountProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, MonetaryBox}
import encry.settings.{Algos, Constants}
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import io.iohk.iodb.LSMStore
import scorex.core.ModifierId
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, PersistentBatchAVLProver}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey, Signature}
import scorex.utils.Random

trait EncryGenerator {

  def timestamp: Long = System.currentTimeMillis()

  def randomAddress: Address = Account(PublicKey @@ Random.randomBytes()).address

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(AccountProposition(address), rndGen.nextLong(), amount, tokenIdOpt)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case (s, box) =>
      s :+ box.id
    }

  def genValidAssetBoxes(secret: PrivateKey25519, amount: Amount, qty: Int): Seq[AssetBox] =
    (0 to qty).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, _) =>
      bxs :+ AssetBox(AccountProposition(Account(secret.publicKeyBytes)), rndGen.nextLong(), amount)
    }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(Random.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genValidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(k.publicImage.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        scala.util.Random.nextLong(), useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }

  def genValidPaymentTxsToAddr(qty: Int, address: Address): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)

    keys.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address))
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        scala.util.Random.nextLong(), useBoxes, address, Props.boxValue)
    }
  }

  def genValidPaymentTxsToAddrWithDiffTokens(qty: Int, address: Address): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val tokens: Seq[ADKey] = (0 until qty).foldLeft(Seq[ADKey]()){
      case (seq, _) => seq :+ (ADKey @@ Random.randomBytes())
    }
    val pksZipTokens: Seq[(PrivateKey25519, ADKey)] = keys.zip(tokens)
    val timestamp: Amount = System.currentTimeMillis()
    pksZipTokens.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address, tokenIdOpt = Some(k._2)))
      TransactionFactory.defaultPaymentTransactionScratch(k._1, Props.txFee,
        timestamp, useBoxes, address, Props.boxValue, tokenIdOpt = Some(k._2))
    }
  }

  def genChainSpendingTxs(qty: Int): Seq[EncryTransaction] = {

    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val timestamp: Amount = System.currentTimeMillis()
    keys.foldLeft(Seq[EncryTransaction]()) { (seq, key) =>
      val useBoxes =
        if(seq.isEmpty) IndexedSeq(genAssetBox(key.publicImage.address))
        else seq.last.newBoxes.map(_.asInstanceOf[MonetaryBox]).toIndexedSeq
      seq :+ TransactionFactory.defaultPaymentTransactionScratch(key, Props.txFee,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }

  def genInvalidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val timestamp: Amount = System.currentTimeMillis()

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(PublicKey25519(PublicKey @@ Random.randomBytes(32)).address))
      TransactionFactory.defaultPaymentTransactionScratch(k, -100,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }

  def genHeader: EncryBlockHeader = {
    val random = new scala.util.Random
    EncryBlockHeader(
      Random.randomBytes(32)(random.nextInt(32)),
      PublicKey25519(PublicKey @@ Random.randomBytes()),
      Signature25519(Signature @@ Random.randomBytes(64)),
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(33),
      Digest32 @@ Random.randomBytes(),
      random.nextLong(),
      random.nextInt(),
      random.nextLong(),
      Constants.Chain.InitialNBits,
      EquihashSolution(Seq(1, 3))
    )
  }

  def genUtxoState: UtxoState = {
    def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
      val p: BatchAVLProver[Digest32, Algos.HF] = new BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
      bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

      val stateStore: LSMStore = new LSMStore(dir, keySize = 32, keepVersions = 10)

      new UtxoState(EncryState.genesisStateVersion, Constants.Chain.GenesisHeight, stateStore, 0L, None) {
        override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Algos.HF] =
          PersistentBatchAVLProver.create(
            p, storage, paranoidChecks = true
          ).get
      }
    }
    val bxs: IndexedSeq[AssetBox] = TestHelper.genAssetBoxes

    val boxHolder: BoxHolder = BoxHolder(bxs)

    utxoFromBoxHolder(boxHolder, FileHelper.getRandomTempDir, None)
  }
}
