package benches

import java.io.File

import akka.actor.ActorRef
import encry.avltree
import encry.avltree.{NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.crypto.equihash.EquihashSolution
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.modifiers.state.box.Box.Amount
import encry.settings.Constants
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.History.Height
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.encryfoundation.common.transaction.Pay2PubKeyAddress
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random
import scala.util.{Random => R}

object Utils {

  def generateNextBlock(prevBlock: Block): Block = {
    val txs = genValidPaymentTxs(100) ++ Seq(coinbaseTransaction)
    val header = Header(
      1.toByte,
      prevBlock.id,
      Digest32 @@ Random.randomBytes(32),
      ADDigest @@ Random.randomBytes(33),
      Payload.rootHash(txs.map(_.id)),
      System.currentTimeMillis(),
      prevBlock.header.height + 1,
      R.nextLong(),
      Difficulty @@ BigInt(1),
      EquihashSolution(Seq(1, 3))
    )
    Block(header, Payload(header.id, txs), None)
  }

  def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new avltree.BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(avltree.Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore = new LSMStore(dir, keySize = 32, keepVersions = 10)

    val persistentProver: avltree.PersistentBatchAVLProver[Digest32, HF] = {
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
      PersistentBatchAVLProver.create(p, storage).get
    }

    new UtxoState(persistentProver, EncryState.genesisStateVersion, Constants.Chain.GenesisHeight, stateStore, 0L, None)
  }

  def getRandomTempDir: File = {
    val dir = java.nio.file.Files.createTempDirectory("encry_test_" + R.alphanumeric.take(15).mkString).toFile
    dir.deleteOnExit()
    dir
  }

  def generateGenesisBlock: Block = {

    val header = genHeader.copy(parentId = Header.GenesisParentId, height = Constants.Chain.GenesisHeight)

    Block(header, Payload(header.id, Seq.empty), None)
  }

  def genHeader: Header = {
    val random = new scala.util.Random
    Header(
      1.toByte,
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(32),
      ADDigest @@ Random.randomBytes(33),
      Digest32 @@ Random.randomBytes(),
      Math.abs(random.nextLong()),
      Math.abs(random.nextInt(10000)),
      random.nextLong(),
      Constants.Chain.InitialDifficulty,
      EquihashSolution(Seq(1, 3))
    )
  }

  def genValidPaymentTxs(qty: Int): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val now = System.currentTimeMillis()

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(k.publicImage.address.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, 4300,
        now + scala.util.Random.nextInt(5000), useBoxes, randomAddress, 1000000)
    }
  }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(Random.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), R.nextLong(), amount, tokenIdOpt)

  def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address

  lazy val coinbaseTransaction: Transaction = {
    TransactionFactory.coinbaseTransactionScratch(secret.publicImage, System.currentTimeMillis(), 10L, 0, Height @@ 100)
  }

  val secrets: Seq[PrivateKey25519] = genKeys(1000)

  def genKeys(qty: Int): Seq[PrivateKey25519] = {
    val rnd: R = new scala.util.Random(Long.MaxValue)
    (0 to qty)
      .foldLeft(Seq[PrivateKey25519]()) { case (acc, _) =>
        val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(
          rnd.alphanumeric.take(32).mkString.getBytes)
        acc :+ PrivateKey25519(keys._1, keys._2)
      }
  }

  val secret: PrivateKey25519 = secrets.head
  val publicKey: PublicKey25519 = secret.publicImage
}