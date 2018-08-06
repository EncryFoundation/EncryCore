package encry.utils

import java.io.File
import akka.actor.ActorRef
import encry.ModifierId
import encry.Address
import encry.crypto.equihash.EquihashSolution
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryTransaction, Pay2PubKeyAddress, TransactionFactory}
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition, MonetaryBox}
import encry.settings.Algos.HF
import encry.settings.{Algos, Constants}
import encry.utils.TestHelper.{Props, rndGen}
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import io.iohk.iodb.LSMStore
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random

trait EncryGenerator {

  def timestamp: Long = System.currentTimeMillis()

  def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), rndGen.nextLong(), amount, tokenIdOpt)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case (s, box) =>
      s :+ box.id
    }

  def genValidAssetBoxes(secret: PrivateKey25519, amount: Amount, qty: Int): Seq[AssetBox] =
    (0 to qty).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, _) =>
      bxs :+ AssetBox(EncryProposition.pubKeyLocked(secret.publicKeyBytes), rndGen.nextLong(), amount)
    }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(Random.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genValidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(k.publicImage.address.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        scala.util.Random.nextLong(), useBoxes, randomAddress, Props.boxValue)
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

  def genValidPaymentTxToAddrWithSpentBoxes(boxes: IndexedSeq[AssetBox], address: Address): EncryTransaction = {
    val key: PrivateKey25519 = genPrivKeys(1).head

    TransactionFactory.defaultPaymentTransactionScratch(key, Props.txFee,
      scala.util.Random.nextLong(), boxes, address, Props.boxValue)
  }

  def genValidPaymentTxsToAddrWithDiffTokens(qty: Int, address: Address): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val tokens: Seq[ADKey] = (0 until qty).foldLeft(Seq[ADKey]()) {
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
      val useBoxes: IndexedSeq[MonetaryBox] = if (seq.isEmpty) IndexedSeq(genAssetBox(key.publicImage.address.address))
        else seq.last.newBoxes.map(_.asInstanceOf[MonetaryBox]).toIndexedSeq
      seq :+ TransactionFactory.defaultPaymentTransactionScratch(key, Props.txFee,
        timestamp, useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genInvalidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val timestamp: Amount = System.currentTimeMillis()

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(PublicKey25519(PublicKey @@ Random.randomBytes(32)).address.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, -100, timestamp, useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genHeader: EncryBlockHeader = {
    val random = new scala.util.Random
    EncryBlockHeader(
      1.toByte,
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(33),
      Digest32 @@ Random.randomBytes(),
      random.nextLong(),
      random.nextInt(),
      random.nextLong(),
      Constants.Chain.InitialDifficulty,
      EquihashSolution(Seq(1, 3))
    )
  }

  def genUtxoState: UtxoState = {
    def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
      val p: BatchAVLProver[Digest32, Algos.HF] = new BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
      bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

      val stateStore: LSMStore = new LSMStore(dir, keySize = 32, keepVersions = 10)

      val persistentProver: PersistentBatchAVLProver[Digest32, HF] = {
        val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
        val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
        PersistentBatchAVLProver.create(p, storage).get
      }

      new UtxoState(persistentProver, EncryState.genesisStateVersion, Constants.Chain.GenesisHeight, stateStore, 0L, None)
    }

    val bxs: IndexedSeq[AssetBox] = TestHelper.genAssetBoxes

    val boxHolder: BoxHolder = BoxHolder(bxs)

    utxoFromBoxHolder(boxHolder, FileHelper.getRandomTempDir, None)
  }
}
