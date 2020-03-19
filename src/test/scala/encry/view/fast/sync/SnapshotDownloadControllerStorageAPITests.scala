//package encry.view.fast.sync
//
//import encry.settings.EncryAppSettings
//import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
//import encry.utils.FileHelper
//import encry.view.fast.sync.SnapshotHolder.SnapshotManifest.ChunkId
//import org.iq80.leveldb.{ DB, Options }
//import org.scalatest.{ Matchers, WordSpecLike }
//import scorex.utils.Random
//
//class SnapshotDownloadControllerStorageAPITests extends WordSpecLike with Matchers {
//
//  val settingsR: EncryAppSettings = EncryAppSettings.read()
//
//  def init: SnapshotDownloadControllerStorageAPI = new SnapshotDownloadControllerStorageAPI {
//    override val storage: DB                = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
//    override val settings: EncryAppSettings = settingsR
//  }
//
//  "Inside SnapshotDownloadControllerStorageAPI class" should {
//    "insert many should insert all ids correctly / split for groups with correct size" in {
//      val api: SnapshotDownloadControllerStorageAPI = init
//      val randomIds: List[ChunkId]                  = (1 to 20001).map(_ => Random.randomBytes()).toList.map(ChunkId @@ _)
//      val groups                                    = randomIds.grouped(settingsR.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
//      val insertionsResult                          = api.insertMany(groups)
//      insertionsResult.isRight shouldBe true
//    }
//    "get next for request should return batch if such exists / remove returned batch" in {
//      val api: SnapshotDownloadControllerStorageAPI = init
//      val randomIds: List[ChunkId]                  = (1 to 5000).map(_ => Random.randomBytes()).toList.map(ChunkId @@ _)
//      val groups                                    = randomIds.grouped(settingsR.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
//      val _                                         = api.insertMany(groups)
//      val groupsL                                   = randomIds.grouped(settingsR.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
//      (0 until groupsL.size).foreach { r =>
//        val res = api.getNextForRequest(r)
//        api.getNextForRequest(r).isLeft shouldBe true
//        res.isRight shouldBe true
//        res.right.get.nonEmpty shouldBe true
//        res.right.get.head.sameElements(groupsL(r).head) shouldBe true
//        res.right.get.forall(j => groupsL(r).exists(_.sameElements(j))) shouldBe true
//        groupsL(r).forall(j => res.right.get.exists(_.sameElements(j))) shouldBe true
//      }
//    }
//  }
//}
