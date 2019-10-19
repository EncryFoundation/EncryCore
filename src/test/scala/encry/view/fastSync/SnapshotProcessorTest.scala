package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import com.typesafe.scalalogging.StrictLogging
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.{SnapshotChunk, SnapshotChunkSerializer}
import encry.view.state.UtxoState
import encry.view.state.avlTree.{AvlTree, Node, NodeSerilalizer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import scorex.utils.Random

class SnapshotProcessorTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings
    with StrictLogging {

  "SnapshotProcessor" should {
    "processNewSnapshot function" should {
      "save new unique snapshot correctly" in {
        val settingsNew                          = settings.copy(snapshotSettings = settings.snapshotSettings.copy(creationHeight = 50))
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settingsNew, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get

        manifest1.rootHash.sameElements(avl1.rootHash) shouldBe true
        manifest1.bestBlockId.sameElements(block1.id) shouldBe true
        manifest1.bestBlockHeight == block1.header.height shouldBe true
        manifest1.chunksKeys.map(ByteArrayWrapper(_)).distinct.size == manifest1.chunksKeys.size shouldBe true

        processor1.potentialManifestsIds.exists(_.sameElements(manifest1.ManifestId)) shouldBe true
        processor1.potentialManifestsIds.size == 1 shouldBe true

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

        val processor2: SnapshotProcessor =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2 = processor2.bestPotentialManifest.get

        manifest1.ManifestId.sameElements(manifest2.ManifestId) shouldBe false
        manifest1.rootHash.sameElements(manifest2.rootHash) shouldBe false

        processor2.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest1.ManifestId)) shouldBe false
        processor2.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest2.ManifestId)) shouldBe true

        processor2.potentialManifestsIds.exists(_.sameElements(manifest1.ManifestId)) shouldBe true
        processor2.potentialManifestsIds.exists(_.sameElements(manifest2.ManifestId)) shouldBe true
        processor2.potentialManifestsIds.size == 2 shouldBe true

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
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
        val processor2: SnapshotProcessor =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2 = processor2.bestPotentialManifest.get
        val processor3: SnapshotProcessor =
          processor2.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest3 = processor3.bestPotentialManifest.get

        manifest3.ManifestId.sameElements(manifest1.ManifestId) shouldBe true
        manifest3.ManifestId.sameElements(manifest2.ManifestId) shouldBe false

        processor3.potentialManifestsIds.size == 2 shouldBe true
      }
      "create a new snapshot with the same block but different root hash" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
        val processor2: SnapshotProcessor =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block1)
        val manifest2 = processor2.bestPotentialManifest.get

        manifest1.ManifestId.sameElements(manifest2.ManifestId) shouldBe false
      }
      "change best potential snapshot correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 501, 1000)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val snapshotProcessor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1 = snapshotProcessor1.bestPotentialManifest.get

        val snapshotProcessor2 =
          snapshotProcessor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)

        snapshotProcessor2.bestPotentialManifest.get.ManifestId.sameElements(manifest1.ManifestId) shouldBe false
      }
      "keep all available potential snapshot correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 501, 1000)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 1501, 2000)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val snapshotProcessor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1 = snapshotProcessor1.bestPotentialManifest.get

        snapshotProcessor1.potentialManifestsIds.size == 1 shouldBe true
        snapshotProcessor1.potentialManifestsIds.exists(_.sameElements(manifest1.ManifestId)) shouldBe true

        val snapshotProcessor2 =
          snapshotProcessor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2 = snapshotProcessor2.bestPotentialManifest.get

        snapshotProcessor2.potentialManifestsIds.size == 2 shouldBe true
        snapshotProcessor2.potentialManifestsIds.exists(_.sameElements(manifest1.ManifestId)) shouldBe true
        snapshotProcessor2.potentialManifestsIds.exists(_.sameElements(manifest2.ManifestId)) shouldBe true

        val snapshotProcessor3 =
          snapshotProcessor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3)
        val manifest3 = snapshotProcessor3.bestPotentialManifest.get

        snapshotProcessor3.potentialManifestsIds.size == 3 shouldBe true
        snapshotProcessor3.potentialManifestsIds.exists(_.sameElements(manifest1.ManifestId)) shouldBe true
        snapshotProcessor3.potentialManifestsIds.exists(_.sameElements(manifest2.ManifestId)) shouldBe true
        snapshotProcessor3.potentialManifestsIds.exists(_.sameElements(manifest3.ManifestId)) shouldBe true
      }
      "Check that different trees give different chunks ids" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 501, 1000)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 1001, 1500)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val processor1                                         = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: Option[SnapshotHolder.SnapshotManifest] = processor1.bestPotentialManifest

        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2  = processor2.bestPotentialManifest

        val processor3 = processor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3)
        val manifest3  = processor3.bestPotentialManifest

        val keys1 = manifest1.get.chunksKeys.map(ByteArrayWrapper(_))
        val keys2 = manifest2.get.chunksKeys.map(ByteArrayWrapper(_))
        val keys3 = manifest3.get.chunksKeys.map(ByteArrayWrapper(_))

        keys1.forall { key =>
          !keys2.contains(key) && !keys3.contains(key)
        } shouldBe true

        keys2.forall { key =>
          !keys1.contains(key) && !keys3.contains(key)
        } shouldBe true

        keys3.forall { key =>
          !keys2.contains(key) && !keys1.contains(key)
        } shouldBe true
      }
    }
    "processNewBlock function" should {
      "skip unnecessary block|process necessary block correctly|remove outdated chunks" in {
        val settingsNew                          = settings.copy(snapshotSettings = settings.snapshotSettings.copy(creationHeight = 50))
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settingsNew, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ settingsNew.snapshotSettings.creationHeight)
        val block3: Block =
          generateGenesisBlock(Height @@ (settingsNew.snapshotSettings.creationHeight + settings.levelDB.maxVersions))

        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get

        val processor2 = processor1.processNewBlock(block1)
        processor2.bestPotentialManifest.nonEmpty shouldBe true
        processor2.actualManifest.isDefined shouldBe false
        processor2.bestPotentialManifest.exists(_.ManifestId.sameElements(manifest1.ManifestId)) shouldBe true

        val processor3: SnapshotProcessor =
          processor2.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest3 = processor3.bestPotentialManifest.get

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

        manifest1.chunksKeys.filterNot(l => processor5.actualManifest.get.chunksKeys.exists(_.sameElements(l))).forall {
          key =>
            processor5.getChunkById(key).isEmpty
        } shouldBe true
      }
      "remove all outdated snapshots correctly with unique snapshot parts" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 501, 1000)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 1001, 1500)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1                                         = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: Option[SnapshotHolder.SnapshotManifest] = processor1.bestPotentialManifest

        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2  = processor2.bestPotentialManifest

        val processor3 = processor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3)
        val manifest3  = processor3.bestPotentialManifest

        val process4 = processor3.processNewBlock(block4)

        /*
          Check, that all previous chunks were removed
         */
        (manifest1.get.chunksKeys ::: manifest2.get.chunksKeys).forall { removedChunkKey =>
          val firstCondition  = process4.getChunkById(removedChunkKey).isEmpty
          val secondCondition = process4.storage.get(StorageKey @@ removedChunkKey).isEmpty
          firstCondition && secondCondition
        } shouldBe true
        /*
          Check, that all previous manifests were removed
         */
        (manifest1.get.ManifestId :: manifest2.get.ManifestId :: Nil).forall { removedManifestKey =>
          val firstCondition  = process4.storage.get(StorageKey @@ removedManifestKey).isEmpty
          val secondCondition = !process4.potentialManifestsIds.exists(_.sameElements(removedManifestKey))
          val thirdCondition  = !process4.actualManifest.get.ManifestId.sameElements(removedManifestKey)
          firstCondition shouldBe true
          secondCondition shouldBe true
          thirdCondition shouldBe true
          firstCondition && secondCondition && thirdCondition
        } shouldBe true
      }
      "remove all outdated snapshots correctly with several same snapshot parts" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 250, 1250)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 500, 1500)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1                                         = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1: Option[SnapshotHolder.SnapshotManifest] = processor1.bestPotentialManifest

        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2  = processor2.bestPotentialManifest

        val processor3 = processor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3)
        val manifest3  = processor3.bestPotentialManifest

        val process4 = processor3.processNewBlock(block4)

        /*
          Check, that all previous chunks were removed excluding new actual chunks
         */
        (manifest1.get.chunksKeys ::: manifest2.get.chunksKeys)
          .filterNot(l => manifest3.get.chunksKeys.exists(_.sameElements(l)))
          .forall { removedChunkKey =>
            val firstCondition  = process4.getChunkById(removedChunkKey).isEmpty
            val secondCondition = process4.storage.get(StorageKey @@ removedChunkKey).isEmpty
            firstCondition && secondCondition
          } shouldBe true
        /*
          Check, that all previous manifests were removed
         */
        (manifest1.get.ManifestId :: manifest2.get.ManifestId :: Nil).forall { removedManifestKey =>
          val firstCondition  = process4.storage.get(StorageKey @@ removedManifestKey).isEmpty
          val secondCondition = !process4.potentialManifestsIds.exists(_.sameElements(removedManifestKey))
          val thirdCondition  = !process4.actualManifest.get.ManifestId.sameElements(removedManifestKey)
          firstCondition shouldBe true
          secondCondition shouldBe true
          thirdCondition shouldBe true
          firstCondition && secondCondition && thirdCondition
        } shouldBe true
      }
      "keep new actual chunks after removing old snapshots with several same chunks" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 501, 1000)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 1001, 1500)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)

        val processor3 = processor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3)
        val manifest3  = processor3.bestPotentialManifest

        val process4 = processor3.processNewBlock(block4)

        /*
          Check, that all new actual chunks exist
         */
        manifest3.get.chunksKeys.forall { id =>
          val firstCondition  = process4.getChunkById(id).nonEmpty
          val secondCondition = process4.storage.get(StorageKey @@ id).nonEmpty
          firstCondition && secondCondition
        } shouldBe true
      }
      "keep new actual manifest after removing old snapshots" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 501, 1000)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 1001, 1500)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)

        val processor3 = processor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3)
        val manifest3  = processor3.bestPotentialManifest

        val process4 = processor3.processNewBlock(block4)

        /*
          Check that new actual exists after removing old
         */
        process4.actualManifest.exists(manifest => manifest.ManifestId.sameElements(manifest3.get.ManifestId)) shouldBe true
        process4.bestPotentialManifest.isEmpty shouldBe true
      }
      "do nothing with block which height is invalid for condition" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val block2: Block =
          generateGenesisBlock(
            Height @@ ((settings.levelDB.maxVersions - 1) + settings.snapshotSettings.creationHeight * 2)
          )

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val processor2 = processor1.processNewBlock(block2)
        processor2.bestPotentialManifest.nonEmpty shouldBe true
        processor2.actualManifest.isEmpty shouldBe true
      }
      "set best potential as actual with right height" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val block3: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val processor3 = processor1.processNewBlock(block3)
        processor3.bestPotentialManifest.isEmpty shouldBe true
        processor3.actualManifest.nonEmpty shouldBe true
      }
      "check that after removing old chunks all actual belong to actual manifest which was inserted first" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 250, 1250)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val manifest1  = processor1.bestPotentialManifest
        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val processor3 = processor2.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val process4 = processor3.processNewBlock(block4)

        process4.actualManifest.get.chunksKeys.flatMap(process4.getChunkById).forall { chunk =>
          chunk.manifestId.toByteArray.sameElements(manifest1.get.ManifestId)
        } shouldBe true
      }
      "check that after removing old chunks all actual belong to actual manifest which was inserted second" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 250, 1250)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)
        val manifest2  = processor2.bestPotentialManifest

        val process4 = processor2.processNewBlock(block4)

        process4.actualManifest.get.chunksKeys.flatMap(process4.getChunkById).forall { chunk =>
          chunk.manifestId.toByteArray.sameElements(manifest2.get.ManifestId)
        } shouldBe true
      }
      "remove all by potential manifests key" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
        val block1: Block = generateGenesisBlock(Height @@ 1)

        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 250, 1250)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val block4: Block =
          generateGenesisBlock(Height @@ (settings.levelDB.maxVersions + settings.snapshotSettings.creationHeight * 2))

        val processor1 = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
        val processor2 = processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2)

        val process4 = processor2.processNewBlock(block4)

        process4.potentialManifestsIds.isEmpty shouldBe true
      }
    }
    "getChunkById" should {
      "return chunk if such exists|return none otherwise" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1: SnapshotProcessor =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
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
    firstAvl
      .insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(),
        boxes.toList,
        List.empty
      )
  }

  def tmpDir: File = FileHelper.getRandomTempDir
}
