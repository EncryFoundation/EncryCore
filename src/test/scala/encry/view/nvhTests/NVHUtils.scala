package encry.view.nvhTests

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.crypto.equihash.EquihashSolution
import encry.modifiers.history.{Block, Header, Payload}
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
import encry.view.state.UtxoState
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import org.iq80.leveldb.Options
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random
import com.typesafe.config.ConfigFactory
import encry.settings.{EncryAppSettings, NodeSettingsReader, SettingsReaders}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.encryfoundation.common.Algos

import scala.util.{Failure, Success, Random => R}

object NVHUtils extends SettingsReaders with NodeSettingsReader {

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
                                       isLast: Boolean = false): Block = {
    val previousHeaderId: ModifierId = prevBlock.map(_.id).getOrElse(Header.GenesisParentId)
    val requiredDifficulty: Difficulty = prevBlock.map(b => history.requiredDifficultyAfter(b.header))
      .getOrElse(Constants.Chain.InitialDifficulty)
    val transactions = txs :+ coinbaseTransaction
    val diff: Difficulty = if (isLast) Difficulty @@ (requiredDifficulty + difficultyDiff) else requiredDifficulty
    val header: Header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.bestHeaderHeight + 1,
      difficulty = diff,
      transactionsRoot = Payload.rootHash(transactions.map(_.id))
    )
//    state.generateProofs(Seq(coinbaseTransaction)) match {
//      case Success((adProof, adDigest)) =>
//        val newHeader = header.copy(adProofsRoot = Algos.hash(adProof), stateRoot = adDigest)
//        Block(newHeader, Payload(header.id, transactions), None)
//      case Failure(_) => Block(header, Payload(header.id, transactions), None)
//    }
    Block(header, Payload(header.id, transactions), None)

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

}
