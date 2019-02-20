package encry.view.wallet.storage

import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbElem, LevelDbFactory, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.wallet.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.crypto.PublicKey25519
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.{Random => ScorexRandom}

import scala.util.{Random, Try}

class WalletStorageSpec extends PropSpec with Matchers {

  val store: LSMStore = new LSMStore(FileHelper.getRandomTempDir)
  val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
  //todo: Get LevelDBSettings from settings
  val vldbInit = VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(100))
  val walletStorage: WalletStorage = new WalletStorage(vldbInit, Set.empty[PublicKey25519])

  property("Complex value unpacking from storage") {


    val values: Seq[Array[Byte]] = Seq(Array.fill(32)(1: Byte), Array.fill(32)(2: Byte), Array.fill(32)(3: Byte))

    val packedValues: VersionalLevelDbValue = VersionalLevelDbValue @@ values.foldLeft(Array[Byte]())(_ ++ _)

    val key: VersionalLevelDbKey = VersionalLevelDbKey @@ scorex.utils.Random.randomBytes()

    walletStorage.store.insert(
      LevelDbElem(
        LevelDBVersion @@ ScorexRandom.randomBytes(),
        List(key -> packedValues),
        Seq.empty
      )
    )

    def parseComplexValue(bytes: Array[Byte], unitLen: Int): Try[Seq[Array[Byte]]] = Try {
      bytes.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
        .ensuring(bytes.length % unitLen == 0, "Value is inconsistent.")
    }

    //todo:  bytearraywrapper -> Array[Byte]
    def readComplexValue(key: ByteArrayWrapper, unitLen: Int): Option[Seq[Array[Byte]]] =
      store.get(key).flatMap { v => parseComplexValue(v.data, unitLen).toOption
      }

    val valuesUnpacked: Seq[Array[Byte]] = readComplexValue(ByteArrayWrapper(key), 32).get

    values.size shouldEqual valuesUnpacked.size

    values.zip(valuesUnpacked).forall(t => t._1 sameElements t._2) shouldBe true
  }
}
