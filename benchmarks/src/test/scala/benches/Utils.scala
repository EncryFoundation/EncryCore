package benches

import java.io.File
import akka.actor.ActorRef
import encry.avltree
import encry.avltree.{NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.crypto.equihash.EquihashSolution
import encry.modifiers.history.{ADProofs, Block, Header, Payload}
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.mempool.{Transaction, TransactionFactory, UnsignedTransaction}
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryProposition, MonetaryBox}
import encry.settings.{Constants, EncryAppSettings, NodeSettings}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.TestHelper.Props
import encry.utils.{Mnemonic, NetworkTimeProvider}
import encry.view.history.EncryHistory
import encry.view.history.History.Height
import encry.view.history.processors.payload.BlockPayloadProcessor
import encry.view.history.processors.proofs.FullStateProofProcessor
import encry.view.history.storage.HistoryStorage
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.encryfoundation.common.transaction.{Input, Pay2PubKeyAddress, Proof, PubKeyLockedContract}
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue, SerializedAdProof}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random
import scala.util.{Random => R}

object Utils {

  def generateGenesisBlockValidForState(state: UtxoState): Block = {
    val txs = Seq(coinbaseTransaction(0))
    val (adProofN: SerializedAdProof, adDigest: ADDigest) = state.generateProofs(txs).get
    val adPN: Digest32 = ADProofs.proofDigest(adProofN)
    val header = genHeader.copy(
      parentId = Header.GenesisParentId,
      adProofsRoot = adPN,
      stateRoot = adDigest,
      height = Constants.Chain.GenesisHeight
    )
    Block(header, Payload(header.id, txs), None)
  }

  def generateGenesisBlockValidForHistory: Block = {
    val header = genHeader.copy(parentId = Header.GenesisParentId, height = Constants.Chain.GenesisHeight)
    Block(header, Payload(header.id, Seq(coinbaseTransaction)), None)
  }

  def generateNextBlockValidForState(prevBlock: Block, state: UtxoState, box: Seq[AssetBox]): Block = {
    val txs: Seq[Transaction] = box.map(b =>
      defaultPaymentTransactionScratch(
        privKey,
        fee = 111,
        timestamp = 11L,
        useBoxes = IndexedSeq(b),
        recipient = randomAddress,
        amount = 10000
      )) ++ Seq(coinbaseTransaction(prevBlock.header.height + 1))
    val (adProofN: SerializedAdProof, adDigest: ADDigest) = state.generateProofs(txs).get
    val adPN: Digest32 = ADProofs.proofDigest(adProofN)
    val header = Header(
      1.toByte,
      prevBlock.id,
      adPN,
      adDigest,
      Payload.rootHash(txs.map(_.id)),
      System.currentTimeMillis(),
      prevBlock.header.height + 1,
      R.nextLong(),
      Difficulty @@ BigInt(1),
      EquihashSolution(Seq(1, 3))
    )
    Block(header, Payload(header.id, txs), None)
  }

  def generateNextBlockValidForHistory(history: EncryHistory,
                                       difficultyDiff: BigInt = 0,
                                       prevBlock: Option[Block],
                                       transactionsNumber: Int): Block = {
    val previousHeaderId: ModifierId = prevBlock.map(_.id).getOrElse(Header.GenesisParentId)
    val requiredDifficulty: Difficulty = prevBlock.map(b => history.requiredDifficultyAfter(b.header))
      .getOrElse(Constants.Chain.InitialDifficulty)
    val txs = genValidPaymentTxs(transactionsNumber) ++ Seq(coinbaseTransaction)
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.bestHeaderHeight + 1,
      difficulty = Difficulty @@ (requiredDifficulty + difficultyDiff),
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )
    Block(header, Payload(header.id, txs), None)
  }

  def genValidPaymentTxs(qty: Int): Seq[Transaction] = {
    val now = System.currentTimeMillis()
    (0 until qty).map { _ =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(privKey.publicImage.address.address))
      defaultPaymentTransactionScratch(privKey, Props.txFee,
        now + scala.util.Random.nextInt(5000), useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), R.nextLong(), amount, tokenIdOpt)

  def utxoFromBoxHolder(bh: BoxHolder,
                        dir: File,
                        nodeViewHolderRef: Option[ActorRef],
                        settings: EncryAppSettings): UtxoState = {
    val p = new avltree.BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(avltree.Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))
    val stateStore = new LSMStore(dir, keySize = 32, keepVersions = 10)
    val persistentProver: avltree.PersistentBatchAVLProver[Digest32, HF] = {
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
      PersistentBatchAVLProver.create(p, storage).get
    }
    new UtxoState(
      persistentProver,
      EncryState.genesisStateVersion,
      Constants.Chain.GenesisHeight,
      stateStore,
      0L,
      None,
      settings,
      None
    )
  }

  def getRandomTempDir: File = {
    val dir = java.nio.file.Files.createTempDirectory("encry_test_" + R.alphanumeric.take(15).mkString).toFile
    dir.deleteOnExit()
    dir
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

  def genHardcodedBox(address: Address, nonce: Long): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), nonce, 10000000L, None)

  def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address

  def coinbaseTransaction(height: Int): Transaction = TransactionFactory.coinbaseTransactionScratch(
    privKey.publicImage,
    System.currentTimeMillis(),
    supply = 10L,
    amount = 1,
    height = Height @@ height
  )

  lazy val coinbaseTransaction: Transaction = {
    TransactionFactory.coinbaseTransactionScratch(
      privKey.publicImage,
      System.currentTimeMillis(),
      10L,
      0,
      Height @@ 100
    )
  }

  val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
  val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))

  def createPrivKey(seed: Option[String]): PrivateKey25519 = {
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(
      Blake2b256.hash(
        seed.map {
          Mnemonic.seedFromMnemonic(_)
        }
          .getOrElse {
            val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
            Mnemonic.seedFromMnemonic(phrase)
          })
    )
    PrivateKey25519(privateKey, publicKey)
  }

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None,
                                       numOfOutputs: Long = 5): Transaction = {
    val pubKey: PublicKey25519 = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useBoxes
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes)))).toIndexedSeq
    val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)
    val directives: IndexedSeq[TransferDirective] =
      if (change > 0) TransferDirective(recipient, amount, tokenIdOpt) +: (0 until numOfOutputs)
        .map(_ => TransferDirective(pubKey.address.address, change / 200, tokenIdOpt))
      else IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, directives)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def generateHistory(settingsEncry: EncryAppSettings, file: File): EncryHistory = {

    val indexStore: LSMStore = new LSMStore(file, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(file, keepVersions = 0)
    val storage: HistoryStorage = new HistoryStorage(indexStore, objectsStore)
    val ntp: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)

    new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
      override protected val settings: EncryAppSettings = settingsEncry
      override protected val nodeSettings: NodeSettings = settings.node
      override protected val historyStorage: HistoryStorage = storage
      override protected val timeProvider: NetworkTimeProvider = ntp
    }
  }

}