package encry.view.wallet.storage

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LogStore}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Random
import scorex.utils.{Random => ScRand}

class WalletStorageSpec extends PropSpec with Matchers {

  val dir: File = new File(s"${System.getProperty("user.dir")}/test-data/wallet2")
  dir.mkdir()

  assert(dir.exists() && dir.isDirectory && dir.listFiles.isEmpty, "dir is invalid.")

  val store = new LogStore(dir)
  val walletStorage = new WalletStorage(store, Set.empty[PublicKey25519Proposition])

  property("Complex value unpacking from storage") {

    val values = Seq(Array.fill(32)(1: Byte), Array.fill(32)(2: Byte), Array.fill(32)(3: Byte))

    val packedValues = new ByteArrayWrapper(values.foldLeft(Array[Byte]())(_ ++ _))

    val key = ByteArrayWrapper(ScRand.randomBytes())

    walletStorage.db.update(Random.nextLong(), Seq(), Seq(key -> packedValues))

    val valuesUnpacked = walletStorage.getAndUnpackComplexValue(key, 32).get

    values.size shouldEqual valuesUnpacked.size

    values.zip(valuesUnpacked).forall(t => t._1 sameElements t._2) shouldBe true
  }
}
