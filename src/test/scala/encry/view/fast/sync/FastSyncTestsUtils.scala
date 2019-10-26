package encry.view.fast.sync

import java.io.File

import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fast.sync.SnapshotHolder.SnapshotManifest
import encry.view.history.History
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import org.iq80.leveldb.Options
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height

object FastSyncTestsUtils extends InstanceFactory with TestNetSettings {

//  def initializeTestState(
//    from: Int = 0,
//    to: Int = 100
//  ): (AvlTree[StorageKey, StorageValue],
//      SnapshotProcessor,
//      SnapshotDownloadController,
//      List[Block],
//      SnapshotManifest,
//      History) = {
//    val firstDir: File = FileHelper.getRandomTempDir
//    val firstStorage: VLDBWrapper = {
//      val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
//      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
//    }
//    val history     = generateDummyHistory(settings)
//    val (_, blocks) = DMUtils.generateBlocks(5, history)
//
//    val boxes: List[(StorageKey, StorageValue)] = blocks
//      .flatMap(_.payload.txs.flatMap(_.newBoxes))
//      .map { bx =>
//        (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)
//      }
//
//    val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
//    val secondAvl = firstAvl
//      .insertAndDeleteMany(
//        StorageVersion @@ Random.randomBytes(),
//        boxes,
//        List.empty
//      )
//
//    val newBlocks = blocks.map(l => Block(l.header.copy(stateRoot = secondAvl.rootHash), l.payload))
//    val history1  = generateDummyHistory(settings)
//    val historyNew1 = newBlocks.foldLeft(history1) {
//      case (history, block) =>
//        history.append(block.header)
//        history.append(block.payload)
//        history.reportModifierIsValid(block)
//    }
//
//    val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
//      .create(settings, tmpDir)
//      .processNewSnapshot(UtxoState(secondAvl, Height @@ 0, settings.constants), newBlocks.last).right.get
//
//    val snapshotDownloadController = SnapshotDownloadController.empty(settings)
//
//    (secondAvl,
//     snapshotProcessor,
//     snapshotDownloadController,
//     newBlocks,
//     snapshotProcessor.manifestById(StorageKey @@ Algos.hash(secondAvl.rootHash ++ newBlocks.last.id)).get,
//     historyNew1)
//  }

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
