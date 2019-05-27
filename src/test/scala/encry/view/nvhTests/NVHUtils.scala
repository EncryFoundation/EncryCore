package encry.view.nvhTests

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.crypto.equihash.EquihashSolution
import encry.modifiers.history.{ADProofs, Block, Header, Payload}
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.network.DeliveryManager
import encry.settings._
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.{FileHelper, Mnemonic, NetworkTimeProvider}
import encry.view.EncryNodeViewHolder
import encry.view.history.EncryHistory
import encry.view.history.History.Height
import encry.view.history.processors.payload.BlockPayloadProcessor
import encry.view.history.processors.proofs.FullStateProofProcessor
import encry.view.history.storage.HistoryStorage
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADValue, SerializedAdProof}
import org.iq80.leveldb.Options
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.StrictLogging
import encry.avltree
import encry.avltree.{NodeParameters, PersistentBatchAVLProver, VersionedAVLStorage}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.{EncryAppSettings, NodeSettingsReader, SettingsReaders}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.StorageType
import encry.storage.iodb.versionalIODB.IODBWrapper
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.encryfoundation.common.transaction.EncryAddress.Address

import scala.util.{Failure, Success, Random => R}

object NVHUtils extends SettingsReaders with NodeSettingsReader with StrictLogging {

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

  val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
  val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))

  val configPath: String = "encry"

  val read: EncryAppSettings = {
    val fakeDir = getRandomTempDir
    ConfigFactory
      .load("test.conf")
      .as[EncryAppSettings]("encry")
      .copy(directory = fakeDir.getAbsolutePath)
  }

  def initDummyNvh(settings: EncryAppSettings,
                   ntp: NetworkTimeProvider)
                  (implicit actorSystem: ActorSystem): TestActorRef[EncryNodeViewHolder[UtxoState]] = {
    val dummyActorsRef = TestProbe()
    TestActorRef[EncryNodeViewHolder[UtxoState]](
      EncryNodeViewHolder.props(
        dummyActorsRef.ref,
        dummyActorsRef.ref,
        dummyActorsRef.ref,
        dummyActorsRef.ref,
        None,
        ntp,
        settings
      )
    )
  }

  def generateHistory(settingsEncry: EncryAppSettings, file: File): EncryHistory = {

    val indexStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vldbInit = VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settingsEncry.levelDB))
    val storage: HistoryStorage = new HistoryStorage(vldbInit)

    val ntp: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)

    new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
      override protected val settings: EncryAppSettings = settingsEncry
      override protected val nodeSettings: NodeSettings = settings.node
      override protected val historyStorage: HistoryStorage = storage
      override protected val timeProvider: NetworkTimeProvider = ntp
      override var bestBlockIdOptCache: Option[ModifierId] = Option.empty[ModifierId]
    }
  }

  def generateNextBlockValidForHistory(history: EncryHistory,
                                       difficultyDiff: BigInt = 0,
                                       prevBlock: Option[Block],
                                       txs: Seq[Transaction],
                                       state: UtxoState,
                                       isLastBlock: Boolean = false): Block = {
    val previousHeaderId: ModifierId = prevBlock.map(_.id).getOrElse(Header.GenesisParentId)
    val requiredDifficulty: Difficulty = prevBlock.map(b => history.requiredDifficultyAfter(b.header))
      .getOrElse(Constants.Chain.InitialDifficulty)
    logger.info(s"islastBlock: $isLastBlock")
    val txs = Seq(coinbaseTransaction(history.bestHeaderHeight + 1))
    val (adProofN: SerializedAdProof, adDigest: ADDigest) = state.generateProofs(txs).get
    val adPN: Digest32 = ADProofs.proofDigest(adProofN)
    val diff = if (isLastBlock) Difficulty @@ (requiredDifficulty + difficultyDiff) else requiredDifficulty
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = prevBlock.map(_.header.height + 1).getOrElse(history.bestHeaderHeight + 1),
      difficulty = diff,
      adProofsRoot = adPN,
      stateRoot = adDigest,
      transactionsRoot = Payload.rootHash(txs.map(_.id)),
      timestamp = if (isLastBlock) System.currentTimeMillis()
      else System.currentTimeMillis() - (Constants.Chain.DesiredBlockInterval.toMillis * 30)
    )
    Block(header, Payload(header.id, txs), None)
  }

  def genAssetBox(address: Address, nonce: Long): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), nonce, 10000000L, None)

  def utxoFromBoxHolder(bh: BoxHolder,
                        dir: File,
                        nodeViewHolderRef: Option[ActorRef],
                        settings: EncryAppSettings,
                        storageType: StorageType): UtxoState = {
    val p = new avltree.BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(avltree.Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))
    val versionalStorage = storageType match {
      case VersionalStorage.IODB =>
        IODBWrapper(new LSMStore(dir, keySize = 32, keepVersions = 10))
      case VersionalStorage.LevelDB =>
        val reopenedLevelDb = LevelDbFactory.factory.open(dir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(reopenedLevelDb, LevelDBSettings(100, 33), keySize = 33))
    }
    val persistentProver: avltree.PersistentBatchAVLProver[Digest32, HF] = {
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedAVLStorage[Digest32] = new VersionedAVLStorage(versionalStorage, np, settings)(Algos.hash)
      PersistentBatchAVLProver.create(p, storage).get
    }
    new UtxoState(
      persistentProver,
      EncryState.genesisStateVersion,
      Constants.Chain.GenesisHeight,
      versionalStorage,
      0L,
      None,
      settings,
      None
    )
  }

  def generateGenesisBlockValidForState(state: UtxoState, timeMultiplier: Long = 40): Block = {
    val txs = Seq(coinbaseTransaction(0))
    val (adProofN: SerializedAdProof, adDigest: ADDigest) = state.generateProofs(txs).get
    val adPN: Digest32 = ADProofs.proofDigest(adProofN)
    val header = genHeader.copy(
      parentId = Header.GenesisParentId,
      adProofsRoot = adPN,
      stateRoot = adDigest,
      timestamp = System.currentTimeMillis() - (Constants.Chain.DesiredBlockInterval.toMillis * timeMultiplier),
      height = Constants.Chain.GenesisHeight,
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )
    Block(header, Payload(header.id, txs), None)
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

  lazy val coinbaseTransaction: Transaction = {
    TransactionFactory.coinbaseTransactionScratch(
      privKey.publicImage,
      System.currentTimeMillis(),
      10L,
      0,
      Height @@ 100
    )
  }

  def coinbaseTransaction(height: Int): Transaction = TransactionFactory.coinbaseTransactionScratch(
    privKey.publicImage,
    System.currentTimeMillis(),
    supply = 10000000L,
    amount = 1L,
    height = Height @@ height
  )

}
