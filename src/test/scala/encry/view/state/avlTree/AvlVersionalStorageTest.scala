package encry.view.state.avlTree

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import org.specs2.mutable.Specification
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._

class AvlVersionalStorageTest extends PropSpec with Matchers with EncryGenerator {

  property("avst should correct process root hash") {

    import encry.utils.implicits.UTXO._

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage = AvlTree[StorageKey, StorageValue](storage)

    val boxes = (0 to 10000).map{i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    //println(boxes.map(bx => Algos.encode(bx._1)).mkString("\n "))

    val startTime = System.currentTimeMillis()

    val newAvl = avlStorage.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )

    println(s"Time = ${(System.currentTimeMillis() - startTime)/1000L} s")

    boxes.forall{case (key, _) => newAvl.contains(key)} shouldBe true
  }

  property("avl test") {

    import encry.utils.implicits.UTXO._
    import cats.instances.int._

    implicit val hashInt = new Hashable[Int] {
      override def hash(value: Int): Array[Byte] = Algos.hash(Ints.toByteArray(value))
    }

    implicit val serInt = new Serializer[Int] {
      override def toBytes(elem: Int): Array[Byte] = Ints.toByteArray(elem)

      override def fromBytes(bytes: Array[Byte]): Int = Ints.fromByteArray(bytes)
    }

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage = AvlTree[Int, Int](storage)

    val boxes = List(3 -> 1, 2 -> 1, 1 -> 1)
    //val boxes = (0 to 100).map(i => i -> i).toList

    //println(boxes.map(bx => Algos.encode(bx._1)).mkString("\n "))

    val startTime = System.currentTimeMillis()

    val newAvl = avlStorage.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes,
      List.empty
    )

    println("------")

    println(newAvl)

    println("right:")

    println(newAvl.rootNode.asInstanceOf[InternalNode[Int, Int]].rightChild.get.asInstanceOf[ShadowNode[Int, Int]].restoreFullNode(storage))

    println(s"Time = ${(System.currentTimeMillis() - startTime)/1000L} s")

    println(Algos.encode(newAvl.rootHash))
  }
}
