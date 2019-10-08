package encry.view.fastSync

import java.io.File
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import scorex.utils.Random

class SnapshotProcessorTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  "SnapshotProcessor" should {
    "create new snapshot correctly" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.empty(settings)

      val firstDir: File = FileHelper.getRandomTempDir
      val firstStorage: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val interval: Int = 80
      val boxes = (0 to interval).map { i =>
        val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
        genAssetBox(addr, i, nonce = i)
      }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

      val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
      val newAvl: AvlTree[StorageKey, StorageValue] = firstAvl.insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(), boxes.take(20).toList, List.empty
      )

      val dummyBlock = generateGenesisBlock(Height @@ 1)
      val dummyBlock1 = generateGenesisBlock(Height @@ 1)

      val newProc = snapshotProcessor.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock)
      val currentBest = newProc._1.bestPotentialManifest.get._1
      val newProc1 = newProc._1.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock1)._1
      newProc1.potentialManifests.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe true
      newProc1.bestPotentialManifest.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe false
      val newProc2 = newProc1.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock)._1
      newProc2.bestPotentialManifest.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe true
    }
    "move consists snapshot to best" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.empty(settings)

      val firstDir: File = FileHelper.getRandomTempDir
      val firstStorage: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val interval: Int = 80
      val boxes = (0 to interval).map { i =>
        val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
        genAssetBox(addr, i, nonce = i)
      }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

      val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
      val newAvl: AvlTree[StorageKey, StorageValue] = firstAvl.insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(), boxes.take(20).toList, List.empty
      )

      val dummyBlock = generateGenesisBlock(Height @@ 1)
      val dummyBlock1 = generateGenesisBlock(Height @@ 1)
      val dummyBlock11 = generateGenesisBlock(Height @@ 100)
      val dummyBlock2 = generateGenesisBlock(Height @@ 1500)

      val newProc = snapshotProcessor.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock)
      val currentBest = newProc._1.bestPotentialManifest.get._1
      val newProc1 = newProc._1.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock1)._1
      newProc1.potentialManifests.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe true
      newProc1.bestPotentialManifest.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe false
      val newProc2 = newProc1.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock)._1
      newProc2.bestPotentialManifest.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe true
      val newProc11 = newProc2.processNewBlock(dummyBlock11)._1
      newProc11.processNewSnapshot(UtxoState(newAvl, settings.constants), dummyBlock)._1
      val newProc3 = newProc2.processNewBlock(dummyBlock2)
      newProc3._1.actualManifest.exists(_._1.ManifestId.sameElements(currentBest.ManifestId)) shouldBe true
      newProc3._1.potentialManifests.isEmpty shouldBe true
    }
  }
}