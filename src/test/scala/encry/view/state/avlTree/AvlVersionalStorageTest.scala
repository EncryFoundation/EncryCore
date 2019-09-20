package encry.view.state.avlTree

import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import org.specs2.mutable.Specification
import scorex.utils.Random

class AvlVersionalStorageTest extends PropSpec with Matchers with EncryGenerator {

  property("avst should correct process root hash") {

    import encry.utils.implicits.UTXO._

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage = AvlVersionalStorage[ADKey, Array[Byte]](storage)

    val boxes = (0 to 1001).map{i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    //println(boxes.map(bx => Algos.encode(bx._1)).mkString("\n "))

    val startTime = System.currentTimeMillis()

    val newAvl = avlStorage.insertWithTree(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )

    println(s"Time = ${(System.currentTimeMillis() - startTime)/1000L} s")

    println(Algos.encode(newAvl.rootHash))
  }
}
