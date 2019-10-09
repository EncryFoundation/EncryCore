package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.SnapshotChunkSerializer
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos
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
    "process new snapshot with new unique snapshot correctly" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)

      val avl1 = createAvl
      val avl2 = createAvl
      val avl3 = createAvl

      val block1 = generateGenesisBlock(Height @@ 1)
      val block2 = generateGenesisBlock(Height @@ 1)
      val block3 = generateGenesisBlock(Height @@ 1)

      val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
      val (manifest1, keys1) = processor1.bestPotentialManifest.get
      keys1.forall(processor1.storage.contains) shouldBe true
      processor1.potentialManifests.exists(_._1.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
      processor1.actualManifest.isEmpty shouldBe true

      val processor2 = processor1.processNewSnapshot(UtxoState(avl2, settings.constants), block2)
      val (manifest2, keys2) = processor2.bestPotentialManifest.get
      keys2.forall(processor2.storage.contains) shouldBe true
      keys1.forall(processor2.storage.contains) shouldBe true
      processor2.potentialManifests.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor2.potentialManifests.exists(_._1.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
      processor2.bestPotentialManifest.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor2.actualManifest.isEmpty shouldBe true

      val processor3 = processor2.processNewSnapshot(UtxoState(avl3, settings.constants), block3)
      val (manifest3, keys3) = processor3.bestPotentialManifest.get
      keys3.forall(processor3.storage.contains) shouldBe true
      keys2.forall(processor3.storage.contains) shouldBe true
      keys1.forall(processor3.storage.contains) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest3.ManifestId)) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
      processor3.bestPotentialManifest.exists(_._1.ManifestId.sameElements(manifest3.ManifestId)) shouldBe true
      processor3.actualManifest.isEmpty shouldBe true

      avl1.close()
      avl2.close()
      avl3.close()
    }

    "update best potential snapshot correctly while duplicated comes" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)

      val avl1 = createAvl
      val avl2 = createAvl

      val block1 = generateGenesisBlock(Height @@ 1)
      val block2 = generateGenesisBlock(Height @@ 1)

      val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
      val (manifest1, _) = processor1.bestPotentialManifest.get

      val processor2 = processor1.processNewSnapshot(UtxoState(avl2, settings.constants), block2)
      val (manifest2, _) = processor2.bestPotentialManifest.get

      val processor3 = processor2.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
      val (manifest3, _) = processor3.bestPotentialManifest.get

      manifest1.ManifestId.sameElements(manifest3.ManifestId) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest3.ManifestId)) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true

      processor3.actualManifest.isEmpty shouldBe true

      avl1.close()
      avl2.close()
    }

    "process new best block correctly" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)

      val avl1 = createAvl
      val avl2 = createAvl

      val block1 = generateGenesisBlock(Height @@ 1)
      val block2 = generateGenesisBlock(Height @@ 1)

      val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
      val (manifest1, keys1) = processor1.bestPotentialManifest.get

      val processor2 = processor1.processNewSnapshot(UtxoState(avl2, settings.constants), block2)
      val (manifest2, keys2) = processor2.bestPotentialManifest.get

      val block3 = generateGenesisBlock(
        Height @@ (settings.snapshotSettings.creationHeight + settings.levelDB.maxVersions - 1))
      val block4 = generateGenesisBlock(
        Height @@ (settings.snapshotSettings.creationHeight + settings.levelDB.maxVersions - 1))
      val block5 = generateGenesisBlock(
        Height @@ (settings.snapshotSettings.creationHeight + settings.levelDB.maxVersions + 1))

      val processor3 = processor2.processNewBlock(block3)

      keys2.forall(processor3.storage.contains) shouldBe true
      keys1.forall(processor3.storage.contains) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor3.potentialManifests.exists(_._1.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
      processor3.bestPotentialManifest.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor3.actualManifest.isEmpty shouldBe true

      val processor4 = processor3.processNewBlock(block4)

      keys2.forall(processor4.storage.contains) shouldBe true
      keys1.forall(processor4.storage.contains) shouldBe true
      processor4.potentialManifests.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor4.potentialManifests.exists(_._1.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
      processor4.bestPotentialManifest.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
      processor4.actualManifest.isEmpty shouldBe true

      val processor5 = processor4.processNewBlock(block5)

      keys2.forall(processor5.storage.contains) shouldBe true
      keys1.forall(!processor5.storage.contains(_)) shouldBe true
      processor5.potentialManifests.isEmpty shouldBe true
      processor5.bestPotentialManifest.isEmpty shouldBe true
      processor5.actualManifest.exists(_._1.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true

      avl1.close()
      avl2.close()
    }

    "restore actual chunks correctly" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)

      val avl1 = createAvl

      val block1 = generateGenesisBlock(Height @@ 1)

      val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)

      val block2 = generateGenesisBlock(
        Height @@ (settings.snapshotSettings.creationHeight + settings.levelDB.maxVersions + 1))

      val processor2 = processor1.processNewBlock(block2)

      val chunks: List[SnapshotHolder.SnapshotChunk] = avl1.initializeSnapshotData(block1)._2
      val hash1: List[ByteArrayWrapper] = chunks.map { e =>
        val a: Array[Byte] = e.nodesList.flatMap(_.toByteArray).toArray
        ByteArrayWrapper(Algos.hash(a))
      }

      val restoredChunks: List[SnapshotHolder.SnapshotChunk] = processor2.restoreActualChunks
        .map(e => SnapshotChunkSerializer.fromProto(SnapshotChunkMessage.parseFrom(e)).get)

      restoredChunks.forall { chunk =>
        val a: Array[Byte] = chunk.nodesList.flatMap(_.toByteArray).toArray
        val hash = ByteArrayWrapper(Algos.hash(a))
        hash1.contains(hash)
      } shouldBe true

      avl1.close()
    }
  }

  def createAvl: AvlTree[StorageKey, StorageValue] = {
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
    firstAvl.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(), boxes.toList, List.empty
    )
  }

  def tmpDir: File = FileHelper.getRandomTempDir
}