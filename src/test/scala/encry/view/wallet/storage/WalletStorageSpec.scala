package encry.view.wallet.storage

import encry.settings.LevelDBSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbElem, LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.wallet.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.crypto.PublicKey25519
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.{Random => ScorexRandom}

import scala.util.{Random, Try}

class WalletStorageSpec extends PropSpec with Matchers {

  val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
  //todo: Get LevelDBSettings from settings
  val vldbInit = VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(100)))
  val walletStorage: WalletStorage = new WalletStorage(vldbInit, Set.empty[PublicKey25519])

  property("Complex value unpacking from storage") {

    val values: Seq[Array[Byte]] = Seq(Array.fill(32)(1: Byte), Array.fill(32)(2: Byte), Array.fill(32)(3: Byte))

    val packedValues: StorageValue = StorageValue @@ values.foldLeft(Array[Byte]())(_ ++ _)

    val key: StorageKey = StorageKey @@ scorex.utils.Random.randomBytes()

    walletStorage.store.insert(
      StorageVersion @@ ScorexRandom.randomBytes(),
      List(key -> packedValues)
    )

    def parseComplexValue(bytes: Array[Byte], unitLen: Int): Try[Seq[Array[Byte]]] = Try {
      bytes.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
        .ensuring(bytes.length % unitLen == 0, "Value is inconsistent.")
    }

    def readComplexValue(key: StorageKey, unitLen: Int): Option[Seq[Array[Byte]]] =
      vldbInit.get(key).flatMap { v => parseComplexValue(v, unitLen).toOption }

    val valuesUnpacked: Seq[Array[Byte]] = readComplexValue(key, 32).get

    values.size shouldEqual valuesUnpacked.size

    values.zip(valuesUnpacked).forall(t => t._1 sameElements t._2) shouldBe true
  }
}
