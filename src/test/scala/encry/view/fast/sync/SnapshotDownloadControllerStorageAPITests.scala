package encry.view.fast.sync

import encry.settings.{ EncryAppSettings, TestNetSettings }
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
import encry.utils.FileHelper
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, Options }
import org.scalatest.{ Matchers, WordSpecLike }
import scorex.utils.Random

class SnapshotDownloadControllerStorageAPITests extends WordSpecLike with Matchers {

  val settingsR: EncryAppSettings = EncryAppSettings.read()

  def init: SnapshotDownloadControllerStorageAPI = new SnapshotDownloadControllerStorageAPI {
    override val storage: DB                = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    override val settings: EncryAppSettings = settingsR
  }

  "Inside SnapshotDownloadControllerStorageAPI class" should {
    "insert many should insert all ids correctly / split for groups with correct size" in {
      val api: SnapshotDownloadControllerStorageAPI = init
      val randomIds: List[Array[Byte]]              = (1 to 20000).map(_ => Random.randomBytes()).toList
      val insertionsResult                          = api.insertMany(randomIds)
      insertionsResult.isRight shouldBe true
    }
    "get next for request should return batch if such exists / remove returned batch" in {
      val api: SnapshotDownloadControllerStorageAPI = init
      val randomIds: List[Array[Byte]]              = (1 to 5000).map(_ => Random.randomBytes()).toList
      val _                                         = api.insertMany(randomIds)
      val groupsL = randomIds.grouped(settingsR.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
      (1 to groupsL.size).foreach { r =>
        val res = api.getNextForRequest(r)
        api.getNextForRequest(r).right.get.isEmpty shouldBe true
        res.isRight shouldBe true
        res.right.get.nonEmpty shouldBe true
        res.right.get.head.sameElements(groupsL(r - 1).head) shouldBe true
        res.right.get.forall(j => groupsL(r - 1).exists(_.sameElements(j))) shouldBe true
        groupsL(r - 1).forall(j => res.right.get.exists(_.sameElements(j))) shouldBe true
      }
      api.isBatchesListNonEmpty.right.get shouldBe false
    }
  }
}
