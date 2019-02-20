package encry.avltree.helpers

import encry.avltree
import encry.avltree.{NodeParameters, PersistentBatchAVLProver, VersionedAVLStorage}
import encry.settings.{EncryAppSettings, LevelDBSettings}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDB, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import io.iohk.iodb.{LSMStore, QuickStore, Store}
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, SerializedAdProof}
import org.iq80.leveldb.Options
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}

trait TestHelper extends FileHelper {

  val enableQuickStore: Boolean = System.getProperty("java.specification.version").startsWith("8")

  def quickTest[R](block: => R): Option[R] = if(enableQuickStore) Some(block)
  else None

  type HF = Blake2b256.type
  type D = Digest32
  type AD = ADDigest
  type P = SerializedAdProof
  type PROVER = avltree.BatchAVLProver[D, HF]
  type VERIFIER = avltree.BatchAVLVerifier[D, HF]
  type PERSISTENT_PROVER = avltree.PersistentBatchAVLProver[D, HF]
  type STORAGE = VersionedAVLStorage[D]

  protected val KL: Int
  protected val VL: Int
  protected val LL: Int

  implicit val hf: HF = Blake2b256

  case class Data(p: PERSISTENT_PROVER, s: STORAGE)

  def createVLDB(keepVersions: Int = 300, keySize: Int = 33): VersionalLevelDB = {
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(keepVersions, keySize), keySize = keySize)
  }

  def createQuickStore(keepVersions: Int = 0): VersionalLevelDB = {
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(keepVersions))
  }

  def createVersionedStorage(store: VersionalLevelDB, settings: EncryAppSettings): STORAGE =
    new VersionedAVLStorage(
      VLDBWrapper(store),
      NodeParameters(KL, Some(VL), LL),
      settings
    )

  def createPersistentProver(storage: STORAGE): PERSISTENT_PROVER = {
    val prover = new avltree.BatchAVLProver[D, HF](KL, Some(VL))
    createPersistentProver(storage, prover)
  }

  def createPersistentProver(storage: STORAGE, prover: PROVER): PERSISTENT_PROVER =
    PersistentBatchAVLProver.create[D, HF](prover, storage, paranoidChecks = true).get

  def createPersistentProverWithVLDB(keepVersions: Int = 10000): PERSISTENT_PROVER = {
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vldbInit = VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(keepVersions, 33), keySize = 33)
    val settings = EncryAppSettings.read
    val storage = createVersionedStorage(vldbInit, settings)
    createPersistentProver(storage)
  }

  def createPersistentProverWithQuick(keepVersions: Int = 0): PERSISTENT_PROVER = {
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vldbInit = VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(keepVersions))
    val settings = EncryAppSettings.read
    val storage = createVersionedStorage(vldbInit, settings)
    createPersistentProver(storage)
  }

  def createVerifier(digest: AD, proof: P): VERIFIER = new avltree.BatchAVLVerifier[D, HF](digest, proof, KL, Some(VL))


  implicit class DigestToBase58String(d: ADDigest) {

    def toBase58: String = Base58.encode(d)
  }

}
