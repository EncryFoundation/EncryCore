package encry.view.wallet.storage

import encry.utils.FileHelper
import encry.view.wallet.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.PublicKey25519
import org.scalatest.{Matchers, PropSpec}
import scala.util.{Random, Try}

class WalletStorageSpec extends PropSpec with Matchers {

  val store: LSMStore = new LSMStore(FileHelper.getRandomTempDir)
  val walletStorage: WalletStorage = new WalletStorage(store, Set.empty[PublicKey25519])

  property("Complex value unpacking from storage") {

    val values: Seq[Array[Byte]] = Seq(Array.fill(32)(1: Byte), Array.fill(32)(2: Byte), Array.fill(32)(3: Byte))

    val packedValues: ByteArrayWrapper = new ByteArrayWrapper(values.foldLeft(Array[Byte]())(_ ++ _))

    val key: ByteArrayWrapper = ByteArrayWrapper(scorex.utils.Random.randomBytes())

    walletStorage.store.update(Random.nextLong(), Seq(), Seq(key -> packedValues))

    def parseComplexValue(bytes: Array[Byte], unitLen: Int): Try[Seq[Array[Byte]]] = Try {
      bytes.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
        .ensuring(bytes.length % unitLen == 0, "Value is inconsistent.")
    }

    def readComplexValue(key: ByteArrayWrapper, unitLen: Int): Option[Seq[Array[Byte]]] =
      store.get(key).flatMap { v => parseComplexValue(v.data, unitLen).toOption
      }

    val valuesUnpacked: Seq[Array[Byte]] = readComplexValue(key, 32).get

    values.size shouldEqual valuesUnpacked.size

    values.zip(valuesUnpacked).forall(t => t._1 sameElements t._2) shouldBe true
  }
}
