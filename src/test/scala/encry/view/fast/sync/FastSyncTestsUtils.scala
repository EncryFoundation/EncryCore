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
import encry.view.state.avlTree.AvlTree
import org.iq80.leveldb.Options
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.utils.Algos

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

    val chunk = SnapshotChunk(tree.rootNode, tree.rootHash)

    val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
      .create(settings.copy(levelDB = settings.levelDB.copy(maxVersions = 10)), tmpDir)
      .initializeApplicableChunksCache(history, history.getBestBlockHeight).right.get
      .updateCache(chunk)

    val mans = snapshotProcessor.potentialManifestsIds
    val sp = snapshotProcessor.createNewSnapshot(
      Algos.hash(blocks.last.header.stateRoot ++ blocks.last.header.id),
      mans,
      List(chunk)
    ).right.get

    val newProc = sp.processNewBlock(additionalBlocks.last, history).right.get

    val snapshotDownloadController = SnapshotDownloadController.empty(settings)


    (tree,
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
