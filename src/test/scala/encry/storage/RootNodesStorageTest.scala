package encry.storage

import java.io.File

import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.avlTree.AvlTree
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{FunSuite, Matchers, PropSpec}
import scorex.utils.Random
import scala.util.{Random => SRandom}

class RootNodesStorageTest extends PropSpec with InstanceFactory with EncryGenerator with Matchers {

  def createAvl: AvlTree[StorageKey, StorageValue] = {
    val firstDir: File = FileHelper.getRandomTempDir
    val firstStorage: VLDBWrapper = {
      val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB.copy(keySize = 33), keySize = 33))
    }
    AvlTree[StorageKey, StorageValue](firstStorage)
  }

  property("testRollback") {
    val avl: AvlTree[StorageKey, StorageValue] = createAvl
    val dir: File = FileHelper.getRandomTempDir
    val levelDb: DB = LevelDbFactory.factory.open(dir, new Options)
    val rootNodesStorage = RootNodesStorage[StorageKey, StorageValue](levelDb, 10)
    val (_, avlAfterInsertions, insertList) =
      (0 to SRandom.nextInt(1000) + 10).foldLeft(rootNodesStorage, avl, List.empty[(Height, (List[(StorageKey, StorageValue)], List[StorageKey]))]) {
      case ((rootStorage, previousAvl, insertionList), height) =>
        val version = StorageVersion @@ Random.randomBytes()
        val toInsert = List(StorageKey @@ Random.randomBytes() -> StorageValue @@ Random.randomBytes())
        val newAvl = previousAvl.insertAndDeleteMany(
          version,
          toInsert,
          List.empty
        )
        val newRootStorage = rootStorage.insert(
          version,
          newAvl.rootNode,
          Height @@ height
        )
        (newRootStorage, newAvl, insertionList :+ (Height @@ height -> (toInsert -> List.empty)))
    }
    val (_, rootNodeRestored) = rootNodesStorage.rollbackToSafePoint(insertList.dropWhile(_._1 != rootNodesStorage.safePointHeight))
    (avlAfterInsertions.rootNode.hash sameElements rootNodeRestored.hash) shouldBe true
  }

  property("testInsert") {

  }

}
