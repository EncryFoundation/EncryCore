package encry.view.wallet.storage

import encry.crypto.PublicKey25519
import encry.utils.FileHelper
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.{Random => ScRand}

import scala.util.Random

class WalletStorageSpec extends PropSpec with Matchers {

  val store = new LSMStore(FileHelper.getRandomTempDir)
  val walletStorage = new WalletStorage(store, Set.empty[PublicKey25519])

  property("Complex value unpacking from storage") {

    val values = Seq(Array.fill(32)(1: Byte), Array.fill(32)(2: Byte), Array.fill(32)(3: Byte))

    val packedValues = new ByteArrayWrapper(values.foldLeft(Array[Byte]())(_ ++ _))

    val key = ByteArrayWrapper(ScRand.randomBytes())

    walletStorage.db.update(Random.nextLong(), Seq(), Seq(key -> packedValues))

    val valuesUnpacked = walletStorage.parseComplexValue(key, 32).get

    values.size shouldEqual valuesUnpacked.size

    values.zip(valuesUnpacked).forall(t => t._1 sameElements t._2) shouldBe true
  }
}
