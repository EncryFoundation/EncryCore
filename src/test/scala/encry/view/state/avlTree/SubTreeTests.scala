package encry.view.state.avlTree

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.ChunksCreator
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import org.specs2.mutable.Specification
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.HashMap


class SubTreeTests extends PropSpec with Matchers with EncryGenerator {

  property("create subtree correctly") {

    import encry.utils.implicits.UTXO._

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage = AvlTree[StorageKey, StorageValue](storage)

    val interval = 100

    val boxes = (0 to interval).map { i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    //println(boxes.map(bx => Algos.encode(bx._1)).mkString("\n "))


    val newAvl: AvlTree[StorageKey, StorageValue] = avlStorage.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )
    val newAvlRoot = newAvl.root

    val startTime = System.currentTimeMillis()
    println("start calculating")
    val chunks: List[ChunksCreator.TmpStateChunk] = newAvl.createSubtrees(List(newAvl.rootNode), List.empty)
    val retChunks = newAvl.retirnSubtrees(chunks)
    println(s"Finished ${(System.currentTimeMillis() - startTime) / 1000} sec")
    chunks.nonEmpty shouldBe true


    val dir2 = FileHelper.getRandomTempDir

    val storage2 = {
      val levelDBInit = LevelDbFactory.factory.open(dir2, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage2 = AvlTree[StorageKey, StorageValue](storage2)

    val jdf: AvlTree[StorageKey, StorageValue] = newAvl.assembleTree(avlStorage2, chunks)

    println(s"${Algos.encode(jdf.root)} -->>> ${Algos.encode(newAvlRoot)}")
    println(s"${jdf.createSubtrees(List(jdf.rootNode), List.empty).size} -->>> ${chunks.size}")

    val ert = retChunks.forall(ch =>
      jdf.contains(ch.key)
    )
    println(ert)
    ert shouldBe true

    jdf.root.sameElements(newAvlRoot) shouldBe true
  }

}
