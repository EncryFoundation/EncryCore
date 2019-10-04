package encry.view.state.avlTree

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.StorageVersion
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.avlTree.utils.implicits.Instances.{Hashable, Serializer}
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._


class AvlTreeStorageRollbackTest extends PropSpec with Matchers with EncryGenerator {

//  property("Avl should correctly work after rollback") {
//
//    import encry.utils.implicits.UTXO._
//    import cats.instances.int._
//
//    implicit val hashInt = new Hashable[Int] {
//      override def hash(value: Int): Array[Byte] = Algos.hash(Ints.toByteArray(value))
//    }
//
//    implicit val serInt = new Serializer[Int] {
//      override def toBytes(elem: Int): Array[Byte] = Ints.toByteArray(elem)
//
//      override def fromBytes(bytes: Array[Byte]): Int = Ints.fromByteArray(bytes)
//    }
//
//
//    val dir = FileHelper.getRandomTempDir
//
//    val storage = {
//      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
//      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
//    }
//
//    val avlStorage = AvlTree[Int, Int](storage)
//
//    val firstBoxes = (0 to 5).map(i => i -> i).toList
//
//    val firstVersion = StorageVersion @@ Random.randomBytes()
//
//    val avl1: AvlTree[Int, Int] = avlStorage.insertAndDeleteMany(
//      firstVersion,
//      firstBoxes,
//      List.empty
//    )
//
//    val secondVersion = StorageVersion @@ Random.randomBytes()
//
//    val secondBoxes = (5 to 10).map(i => i -> i).toList
//
//    val avl2 = avl1.insertAndDeleteMany(
//      secondVersion,
//      secondBoxes,
//      List.empty
//    )
//
//    val avlAfterRollback = avl2.rollbackTo(firstVersion).get
//
//    val avl3 = avlAfterRollback.insertAndDeleteMany(
//      secondVersion,
//      secondBoxes,
//      List.empty
//    )
//  }

}
