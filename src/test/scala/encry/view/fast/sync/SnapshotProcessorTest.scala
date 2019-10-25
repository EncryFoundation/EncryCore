package encry.view.fast.sync

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import encry.utils.FileHelper
import encry.network.DeliveryManagerTests.DMUtils
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.Options
import org.scalatest.{ Matchers, OneInstancePerTest, WordSpecLike }
import scorex.utils.Random

class SnapshotProcessorTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings
    with StrictLogging {

  "Snapshot processor" should {
    "process new snapshot function" should {
      "correctly create new snapshot if such doesn't exist" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)
        val avl3: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 10, 30)

        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ 1)
        val block3: Block = generateGenesisBlock(Height @@ 1)

        val processor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1).right.get

        val processor2 =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2).right.get

        val processor3 =
          processor2.processNewSnapshot(UtxoState(avl3, Height @@ 0, settings.constants), block3).right.get

        val id1 = Algos.hash(avl1.rootHash ++ block1.id)
        val id2 = Algos.hash(avl2.rootHash ++ block2.id)
        val id3 = Algos.hash(avl3.rootHash ++ block3.id)

        val listIds = id1 :: id2 :: id3 :: Nil

        processor3.potentialManifestsIds.forall { id =>
          listIds.exists(_.sameElements(id))
        } shouldBe true

        listIds.forall { id =>
          processor3.potentialManifestsIds.exists(_.sameElements(id))
        } shouldBe true

        val ids1 = processor3.manifestById(StorageKey @@ id1).get.chunksKeys
        val ids2 = processor3.manifestById(StorageKey @@ id2).get.chunksKeys
        val ids3 = processor3.manifestById(StorageKey @@ id3).get.chunksKeys

        ids1.forall { id =>
          processor3.getChunkById(id).nonEmpty
        } shouldBe true

        ids2.forall { id =>
          processor3.getChunkById(id).nonEmpty
        } shouldBe true

        ids3.forall { id =>
          processor3.getChunkById(id).nonEmpty
        } shouldBe true

      }
      "skip snapshot creation if such already exists" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)

        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ 1)

        val processor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1).right.get

        val processor2 =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), block2).right.get

        val processor3 =
          processor2.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1).right.get

        val id1 = Algos.hash(avl1.rootHash ++ block1.id)
        val id2 = Algos.hash(avl2.rootHash ++ block2.id)

        processor3.potentialManifestsIds.size == 2 shouldBe true

        processor3.potentialManifestsIds.forall { id =>
          id.sameElements(id1) || id.sameElements(id2)
        } shouldBe true
      }
    }
    "process new block function" should {
      "process block correctly if condition == 0" in {
        val sn = settings.copy(snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 4),
                               levelDB = settings.levelDB.copy(maxVersions = 1))
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(sn, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)

        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ (4 + sn.levelDB.maxVersions))

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(5, history)
        val newBlocks   = blocks.map(b => Block(b.header.copy(stateRoot = avl2.rootHash), b.payload))

        logger.info(s"${newBlocks.map(_.encodedId)}")

        val history1 = generateDummyHistory(settings)
        val historyNew1 = newBlocks.foldLeft(history1) {
          case (history, block) =>
            history.append(block.header)
            history.append(block.payload)
            history.reportModifierIsValid(block)
        }

        val processor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1).right.get

        val processor2 =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), newBlocks.last).right.get

        val id1 = Algos.hash(avl1.rootHash ++ block1.id)
        val ids1 = processor2.manifestById(StorageKey @@ id1).get.chunksKeys

        val processor3 =
          processor2.processNewBlock(block2, historyNew1).right.get

        val id2 = Algos.hash(avl2.rootHash ++ newBlocks.last.id)

        processor3.potentialManifestsIds.isEmpty shouldBe true

        logger.info(s"${Algos.encode(id2)}")

        processor3.manifestById(StorageKey @@ id1).isEmpty shouldBe true
        processor3.manifestById(StorageKey @@ id2).nonEmpty shouldBe true

        val ids2 = processor3.manifestById(StorageKey @@ id2).get.chunksKeys

        ids2.forall { id =>
          processor3.getChunkById(id).nonEmpty
        } shouldBe true

        val ids11 = ids1.filterNot(l => ids2.exists(_.sameElements(l)))

        ids11.forall { id =>
          processor3.getChunkById(id).isEmpty
        } shouldBe true
      }
      "skip block if condition != 0" in {
        val sn = settings.copy(snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 50),
          levelDB = settings.levelDB.copy(maxVersions = 1))
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(sn, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val avl2: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 5, 25)

        val block1: Block = generateGenesisBlock(Height @@ 1)
        val block2: Block = generateGenesisBlock(Height @@ (4 + sn.levelDB.maxVersions))

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(5, history)
        val newBlocks   = blocks.map(b => Block(b.header.copy(stateRoot = avl2.rootHash), b.payload))

        logger.info(s"${newBlocks.map(_.encodedId)}")

        val history1 = generateDummyHistory(settings)
        val historyNew1 = newBlocks.foldLeft(history1) {
          case (history, block) =>
            history.append(block.header)
            history.append(block.payload)
            history.reportModifierIsValid(block)
        }

        val processor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1).right.get

        val processor2 =
          processor1.processNewSnapshot(UtxoState(avl2, Height @@ 0, settings.constants), newBlocks.last).right.get

        val processor3 =
          processor2.processNewBlock(block2, historyNew1).right.get

        val id1 = Algos.hash(avl1.rootHash ++ block1.id)
        val id2 = Algos.hash(avl2.rootHash ++ newBlocks.last.id)

        processor3.potentialManifestsIds.size == 2 shouldBe true

        processor3.manifestById(StorageKey @@ id1).nonEmpty shouldBe true
        processor3.manifestById(StorageKey @@ id2).nonEmpty shouldBe true

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
