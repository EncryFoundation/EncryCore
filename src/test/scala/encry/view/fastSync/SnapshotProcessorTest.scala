package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.{ SnapshotChunk, SnapshotChunkSerializer }
import encry.view.state.UtxoState
import encry.view.state.avlTree.{ AvlTree, Node, NodeSerilalizer }
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.Options
import org.scalatest.{ Matchers, OneInstancePerTest, WordSpecLike }
import scorex.utils.Random

class SnapshotProcessorTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {

  "SnapshotProcessor" should {
    "processNewSnapshot function" should {
      "save new unique snapshot correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get

        manifest1.rootHash.sameElements(avl1.rootHash) shouldBe true
        manifest1.bestBlockId.sameElements(block1.id) shouldBe true
        manifest1.bestBlockHeight == block1.header.height shouldBe true
        manifest1.chunksKeys.map(ByteArrayWrapper(_)).distinct.size == manifest1.chunksKeys.size shouldBe true

        processor1.potentialManifests.exists(_.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
        processor1.potentialManifests.size == 1 shouldBe true

        manifest1.chunksKeys.forall { chunkId =>
          processor1.storage.get(StorageKey @@ chunkId).isDefined
        } shouldBe true
        manifest1.chunksKeys
          .flatMap(chunkId => processor1.storage.get(StorageKey @@ chunkId))
          .forall { res =>
            val chunk: SnapshotChunk = SnapshotChunkSerializer.fromProto(SnapshotChunkMessage.parseFrom(res)).get
            val nodes: List[Node[StorageKey, StorageValue]] =
              chunk.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_))
            nodes.forall(node => avl1.contains(node.key))
          } shouldBe true

        val processor2: SnapshotProcessor = processor1.processNewSnapshot(UtxoState(avl2, settings.constants), block2)
        val manifest2                     = processor2.bestPotentialManifest.get

        manifest1.ManifestId.sameElements(manifest2.ManifestId) shouldBe false
        manifest1.rootHash.sameElements(manifest2.rootHash) shouldBe false

        processor2.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest1.ManifestId)) shouldBe false
        processor2.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true

        processor2.potentialManifests.exists(_.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true
        processor2.potentialManifests.exists(_.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true
        processor2.potentialManifests.size == 2 shouldBe true

        manifest2.chunksKeys.forall { chunkId =>
          processor2.storage.get(StorageKey @@ chunkId).isDefined
        } shouldBe true
        manifest2.chunksKeys
          .flatMap(chunkId => processor2.storage.get(StorageKey @@ chunkId))
          .forall { res =>
            val chunk: SnapshotChunk = SnapshotChunkSerializer.fromProto(SnapshotChunkMessage.parseFrom(res)).get
            val nodes: List[Node[StorageKey, StorageValue]] =
              chunk.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_))
            nodes.forall(node => avl2.contains(node.key))
          } shouldBe true

        manifest2.chunksKeys.map(ByteArrayWrapper(_)).distinct.size == manifest2.chunksKeys.size shouldBe true
      }
      "update an existing snapshot as better correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
        val processor2: SnapshotProcessor              = processor1.processNewSnapshot(UtxoState(avl2, settings.constants), block2)
        val manifest2                                  = processor2.bestPotentialManifest.get
        val processor3: SnapshotProcessor              = processor2.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
        val manifest3                                  = processor3.bestPotentialManifest.get

        manifest3.ManifestId.sameElements(manifest1.ManifestId) shouldBe true
        manifest3.ManifestId.sameElements(manifest2.ManifestId) shouldBe false

        processor3.potentialManifests.size == 2 shouldBe true
      }
      "create a new snapshot with the same block but different root hash" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
        val processor2: SnapshotProcessor              = processor1.processNewSnapshot(UtxoState(avl2, settings.constants), block1)
        val manifest2                                  = processor2.bestPotentialManifest.get

        manifest1.ManifestId.sameElements(manifest2.ManifestId) shouldBe false
      }
    }
    "processNewBlock function" should {
      "skip unnecessary block|process necessary block correctly|remove outdated chunks" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ settings.snapshotSettings.creationHeight)
        val block3: Block =
          generateGenesisBlock(Height @@ (settings.snapshotSettings.creationHeight + settings.levelDB.maxVersions))

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get

        val processor2 = processor1.processNewBlock(block1)
        processor2.bestPotentialManifest.nonEmpty shouldBe true
        processor2.actualManifest.isDefined shouldBe false
        processor2.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true

        val processor3: SnapshotProcessor = processor2.processNewSnapshot(UtxoState(avl2, settings.constants), block2)
        val manifest3                     = processor3.bestPotentialManifest.get

        val processor4 = processor3.processNewBlock(block2)
        processor4.bestPotentialManifest.nonEmpty shouldBe true
        processor4.actualManifest.isDefined shouldBe false
        processor4.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest3.ManifestId)) shouldBe true

        val processor5 = processor3.processNewBlock(block3)
        processor5.bestPotentialManifest.nonEmpty shouldBe false
        processor5.actualManifest.isDefined shouldBe true
        processor5.actualManifest.exists(_.ManifestId.sameElements(manifest3.ManifestId)) shouldBe true

        processor5.actualManifest.get.chunksKeys.forall { key =>
          processor5.getChunkById(key).isDefined
        } shouldBe true

        manifest1.chunksKeys.forall { key =>
          processor5.getChunkById(key).isEmpty
        } shouldBe true
      }
    }
    "getChunkById" should {
      "return chunk if such exists|return none otherwise" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, settings.constants), block1)
        val randomKeys = (0 to 20).map(_ => StorageKey @@ Random.randomBytes()).toList

        processor1.bestPotentialManifest.get.chunksKeys.forall(l => processor1.getChunkById(l).isDefined) shouldBe true

        randomKeys.forall(l => processor1.getChunkById(l).isDefined) shouldBe false
      }
    }
  }

  def createAvl(address: String, from: Int, to: Int): AvlTree[StorageKey, StorageValue] = {
    val firstDir: File = FileHelper.getRandomTempDir
    val firstStorage: VLDBWrapper = {
      val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    val boxes: IndexedSeq[(StorageKey, StorageValue)] = (from to to)
      .map(i => genAssetBox(address, i, nonce = i))
      .map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
    firstAvl.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )
  }

  def tmpDir: File = FileHelper.getRandomTempDir
}
