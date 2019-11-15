package encry.view.fast.sync

import java.io.File

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fast.sync.SnapshotHolder.{SnapshotChunk, SnapshotManifest}
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import org.iq80.leveldb.Options
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height

object FastSyncTestsUtils extends InstanceFactory with TestNetSettings {

  def initializeTestState(initialBlocksQty: Int = 90,
                          additionalBlocksQty: Int = 10,
                          addressOpt: Option[Address] = None
  ): (AvlTree[StorageKey, StorageValue],
      SnapshotProcessor,
      SnapshotDownloadController,
      List[Block],
      SnapshotManifest,
      History) = {

    val (history, blocks, tree) = DMUtils.generateBlocksWithTree(initialBlocksQty, generateDummyHistory(settings), None, addressOpt)
    val (_, additionalBlocks, _) = DMUtils.generateBlocksWithTree(additionalBlocksQty, history, Some(tree), addressOpt)

    val sn = settings
      .copy(
        levelDB = settings.levelDB.copy(maxVersions = 5),
        snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 10)
      )

    val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
      .create(sn, tmpDir)
      .initializeApplicableChunksCache(history, history.getBestBlockHeight).right.get

    val emptyTree = AvlTree[StorageKey, StorageValue] {
      VLDBWrapper(VersionalLevelDBCompanion(LevelDbFactory.factory.open(tmpDir, new Options), settings.levelDB, keySize = 32))
    }

    val (newProc, newTree) = blocks.foldLeft((snapshotProcessor, emptyTree)) { case ((proc, currentTree), block) =>
      import encry.utils.implicits.UTXO._

      val combinedStateChange: UtxoState.StateChange = combineAll(block.payload.txs.map(UtxoState.tx2StateChange).toList)

      val resultingTree = currentTree.insertAndDeleteMany(
        StorageVersion !@@ block.id,
        combinedStateChange.outputsToDb.toList,
        combinedStateChange.inputsToDb.toList,
        Height @@ block.header.height
      )

      if ((block.header.height - sn.levelDB.maxVersions) % sn.snapshotSettings.newSnapshotCreationHeight == 0) {
        (proc.processNewBlock(block, history).right.get, resultingTree)
      } else if (block.header.height % sn.snapshotSettings.newSnapshotCreationHeight == 0) {
        val chunks: List[SnapshotChunk] =
          AvlTree.getChunks(resultingTree.rootNode, currentChunkHeight = 1, resultingTree.storage)
        val potentialManifestId: Array[Byte] = Algos.hash(resultingTree.rootHash ++ block.id)
        (snapshotProcessor.createNewSnapshot(potentialManifestId, chunks).right.get, resultingTree)
      } else (proc, resultingTree)
    }

    val snapshotDownloadController = SnapshotDownloadController.empty(settings)


    (newTree,
     newProc,
     snapshotDownloadController,
     blocks ++ additionalBlocks,
     newProc.actualManifest.get,
     history)
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
