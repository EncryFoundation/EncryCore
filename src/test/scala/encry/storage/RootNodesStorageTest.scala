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
import org.iq80.leveldb.{DB, Options, ReadOptions}
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
    val dir: File = FileHelper.getRandomTempDir
    val levelDb: DB = LevelDbFactory.factory.open(dir, new Options)
    AvlTree[StorageKey, StorageValue](firstStorage, RootNodesStorage.emptyRootStorage[StorageKey, StorageValue])
  }

  property("testRollback") {
    val avl: AvlTree[StorageKey, StorageValue] = createAvl
    val dir: File = FileHelper.getRandomTempDir
    val levelDb: DB = LevelDbFactory.factory.open(dir, new Options)
    val batch1 = levelDb.createWriteBatch()
    val readOptions1 = new ReadOptions()
    val rootNodesStorage = RootNodesStorage[StorageKey, StorageValue](levelDb, 10, dir)
    val (_, avlAfterInsertions, insertList) =
      (0 to SRandom.nextInt(1000) + 10).foldLeft(rootNodesStorage, avl, List.empty[(Height, (List[(StorageKey, StorageValue)], List[StorageKey]))]) {
      case ((rootStorage, previousAvl, insertionList), height) =>
        val version = StorageVersion @@ Random.randomBytes()
        val toInsert = (0 to SRandom.nextInt(100)).foldLeft(List.empty[(StorageKey, StorageValue)]) {
          case (list, _) => (StorageKey @@ Random.randomBytes() -> StorageValue @@ Random.randomBytes()) :: list
        }
        val previousInsertions = insertionList.lastOption.map(_._2._1).getOrElse(List.empty[(StorageKey, StorageValue)])
        val deletions = previousInsertions.take(1).map(_._1)
        val newAvl = previousAvl.insertAndDeleteMany(
          version,
          toInsert,
          deletions
        )
        val newRootStorage = rootStorage.insert(
          version,
          newAvl.rootNode,
          Height @@ height
        )
        (newRootStorage, newAvl, insertionList :+ (Height @@ height -> (toInsert -> deletions)))
    }
    val (_, rootNodeRestored) = rootNodesStorage.rollbackToSafePoint(insertList.dropWhile(_._1 != rootNodesStorage.safePointHeight).drop(1))
    (avlAfterInsertions.rootNode.hash sameElements rootNodeRestored.hash) shouldBe true
  }
}
