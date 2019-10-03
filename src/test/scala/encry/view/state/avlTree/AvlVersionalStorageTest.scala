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
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.HashMap

class AvlVersionalStorageTest extends PropSpec with Matchers with EncryGenerator {

  property("avst should correct process root hash") {

    import encry.utils.implicits.UTXO._

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage = AvlTree[StorageKey, StorageValue](storage)

    val interval = 500

    val boxes = (0 to interval).map{i =>
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

    boxes.forall{case (key, _) =>
      println("test:" + Algos.encode(Algos.hash(Algos.hash(key))))
      newAvl.contains(key)} shouldBe true

    val newBoxes = (interval to interval*2).map{i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    println("second insert!")
    val newAvl2 = newAvl.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      newBoxes.toList,
      boxes.map{case (key, _) => key}.toList
    )

    newBoxes.forall{case (key, _) => newAvl.contains(key)} shouldBe true

    val anotherNewBoxes = (interval*2 to interval*3).map{i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))
    println("third insert!")
    val newAvl3 = newAvl2.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      anotherNewBoxes.toList,
      newBoxes.map{case (key, _) => key}.toList
    )
  }

  property("test") {
    val testHash = HashMap(ByteArrayWrapper(Ints.toByteArray(1)) -> 1,ByteArrayWrapper(Ints.toByteArray(1)) -> 2)
//    val test1 = testHash + (ByteArrayWrapper(Ints.toByteArray(1)) -> 2)
//    val test2 = testHash + (ByteArrayWrapper(Ints.toByteArray(1)) -> 3)
//    val test3 = testHash + (ByteArrayWrapper(Ints.toByteArray(1)) -> 4)
//    val test4 = testHash + (ByteArrayWrapper(Ints.toByteArray(1)) -> 5)
    val test2Hash = testHash ++ List(ByteArrayWrapper(Ints.toByteArray(1)) -> 3)
    println(test2Hash)
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

    val interval = 10

    val boxes = (0 to interval).map(i => i -> i).toList
    //val boxes = (0 to 100).map(i => i -> i).toList

    //println(boxes.map(bx => Algos.encode(bx._1)).mkString("\n "))

    val startTime = System.currentTimeMillis()

    val newAvl = avlStorage.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes,
      List.empty
    )

    boxes.forall{case (key, _) => newAvl.contains(key)} shouldBe true

    println(newAvl.rootNode)

//    val newBoxes = (interval to interval*2).map(i => i -> i).toList
//
//    val newAvl2 = newAvl.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      newBoxes,
//      boxes.map(_._1)
//    )

//    println(newAvl2.rootNode)

//    val newBoxe2 = (interval*2 to interval*3).map(i => i -> i).toList
//
//    val newAvl3 = newAvl2.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      newBoxe2,
//      newBoxes.map(_._1)
//    )
//
//    println(newAvl3.rootNode)

//    println("------")
//
//    println(newAvl)
//
//    println("right:")
//
//    println(newAvl.rootNode.asInstanceOf[InternalNode[Int, Int]].rightChild.get.asInstanceOf[ShadowNode[Int, Int]].restoreFullNode(storage))
//
//    println(s"Time = ${(System.currentTimeMillis() - startTime)/1000L} s")
//
//    println(Algos.encode(newAvl.rootHash))
  }
}
