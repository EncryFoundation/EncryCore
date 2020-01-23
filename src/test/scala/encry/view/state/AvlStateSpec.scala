package encry.view.state

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.UtxoState.{initialStateBoxes, logger}
import encry.view.state.avlTree.AvlTree
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.utils.Algos
import scorex.utils.Random
import scala.util.{Random => SRand}

class AvlStateSpec extends PropSpec with Matchers with EncryGenerator with StrictLogging {

  property("Insertion test") {

    val settings = LevelDBSettings(100, 32, 33)

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.copy(keySize = 33), keySize = 33))
     // IODBWrapper(new LSMStore(FileHelper.getRandomTempDir, keepVersions = 100, keySize = 33))
    }
    //nitialStateBoxes.map(bx => logger.info(s"Insert ${Algos.encode(AvlTree.elementKey(bx.id))}"))
    storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      initialStateBoxes.map(bx => (StorageKey !@@ AvlTree.elementKey(bx.id), StorageValue @@ bx.bytes))
    )


    val tree = AvlTree[StorageKey, StorageValue](storage)

//
//    val newBoxes = genValidAssetBoxes(privKey, 10, initialStateBoxes.length).toList
//
//    val newTree = tree.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      newBoxes.map(box => StorageKey @@ box.id -> StorageValue @@ box.bytes),
//      initialStateBoxes.map(bx => StorageKey @@ bx.id)
//    )
//    logger.info("===================END1==============")
//
//    val anotherBoxes = genValidAssetBoxes(privKey, 10, newBoxes.length).toList
//
//    val anotherTree = newTree.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      anotherBoxes.map(box => StorageKey @@ box.id -> StorageValue @@ box.bytes),
//      newBoxes.map(bx => StorageKey @@ bx.id)
//    )
//
//    logger.info("===================END2==============")
//
//    val anotherBoxes1 = genValidAssetBoxes(privKey, 10, anotherBoxes.length).toList
//
//    val anotherTree1 = anotherTree.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      anotherBoxes1.map(box => StorageKey @@ box.id -> StorageValue @@ box.bytes),
//      anotherBoxes.map(bx => StorageKey @@ bx.id)
//    )
//
//    logger.info("===================END3==============")
//
//    val anotherBoxes2 = genValidAssetBoxes(privKey, 10, anotherBoxes1.length).toList
//
//    val anotherTree2 = anotherTree1.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      anotherBoxes2.map(box => StorageKey @@ box.id -> StorageValue @@ box.bytes),
//      anotherBoxes.map(bx => StorageKey @@ bx.id)
//    )

    val (boxesToDelete, newTree) = (0 to 1000).foldLeft(initialStateBoxes, tree) {
      case ((boxes, tree), _) =>
        val newBoxes = genValidAssetBoxes(privKey, 10, 1000).toList
        newBoxes -> tree.insertAndDeleteMany(
          StorageVersion @@ Random.randomBytes(),
          newBoxes.map(box => StorageKey @@ box.id -> StorageValue @@ box.bytes),
          boxes.take(SRand.nextInt(boxes.length)).map(bx => StorageKey @@ bx.id)
        )
    }

    logger.info("===================END1==============")

    (0 to 3).foldLeft(boxesToDelete.take(1), newTree) {
      case ((boxes, tree), _) =>
        val newBoxes = genValidAssetBoxes(privKey, 10, boxes.length).toList
        newBoxes -> tree.insertAndDeleteMany(
          StorageVersion @@ Random.randomBytes(),
          newBoxes.map(box => StorageKey @@ box.id -> StorageValue @@ box.bytes),
          boxes.map(bx => StorageKey @@ bx.id)
        )
    }
  }


}
