package encry.view.state.avlTree

import cats.Order
import encry.utils.{EncryGenerator, FileHelper}
import org.scalatest.{Matchers, PropSpec}
import cats.instances.int._
import cats.instances.string._
import com.google.common.primitives.Ints
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.view.state.avlTree.utils.implicits.{Hashable, Serializer}
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.Options

import scala.util.Random

class AvlTreeTest extends PropSpec with Matchers with EncryGenerator {

  implicit val hashInt: Hashable[Int] = new Hashable[Int] {
    override def hash(value: Int): Array[Byte] = Algos.hash(value.toString)
  }

  implicit val ser: Serializer[Int] = new Serializer[Int] {
    override def toBytes(elem: Int): Array[Byte] = Ints.toByteArray(elem)

    override def fromBytes(bytes: Array[Byte]): Int = Ints.fromByteArray(bytes)
  }

  property("avl should contains all inserted elems"){

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avl = AvlTree[Int, Int](storage)

    val elems = (0 to 1000).map(_ => Random.nextInt()).toList

    val startTime = System.currentTimeMillis()

    val (newAvl, _, _) = avl.insertMany(elems.map(i => i -> i))

    println(s"insert time: ${(System.currentTimeMillis() - startTime)/1000L} s")

    println(Algos.encode(newAvl.rootHash))

    println(newAvl.rootNode)

    elems.forall(newAvl.containsInTree) shouldBe true
  }
//
//  property("avl should shouldn't contains deleted elems"){
//
//    val avl = AvlTree[Int, Int]()
//
//    val elems = (0 to 100).map(_ => Random.nextInt())
//
//    val newAvl = elems.foldLeft(avl){
//      case (prevTree, elemToInsert) => prevTree.insert(elemToInsert, elemToInsert)
//    }
//
//    val toDelete = elems.take(Random.nextInt(elems.length - 1))
//
//    val avlAfterDeletions = toDelete.foldLeft(newAvl){ case (prevTree, elemToDelete) => prevTree.delete(elemToDelete)}
//
//    toDelete.forall { elem => !avlAfterDeletions.contains(elem) } shouldBe true
//    elems.diff(toDelete).forall(avlAfterDeletions.contains) shouldBe true
//  }
}
