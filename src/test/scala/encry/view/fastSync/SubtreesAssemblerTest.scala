package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.SnapshotChunkSerializer
import encry.view.state.UtxoState
import encry.view.state.avlTree.{AvlTree, InternalNode, NodeSerilalizer }
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.TaggedTypes.Height
import scorex.utils.Random
import scala.util.{ Random => random }
import cats.syntax.either._

class SubtreesAssemblerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings
    with StrictLogging {

//  "Trees validator" should {
//    "check tree assembly(with correct nodes)" in {
//      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
//      val avl1: AvlTree[StorageKey, StorageValue] =
//        createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
//      val block1: Block = generateGenesisBlock(Height @@ 1)
//
//      val processor1: SnapshotProcessor =
//        snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
//      val manifest: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
//
//      val newTree: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](
//        manifest,
//        VLDBWrapper(
//          VersionalLevelDBCompanion(LevelDbFactory.factory.open(tmpDir, new Options), settings.levelDB, keySize = 32)
//        )
//      )
//
//      val newTree1: AvlTree[StorageKey, StorageValue] = manifest.chunksKeys.foldLeft(newTree) {
//        case (tree, key) =>
//          val chunk: SnapshotChunkMessage         = processor1.getChunkById(key).get
//          val nodes: SnapshotHolder.SnapshotChunk = SnapshotChunkSerializer.fromProto(chunk).get
//          val treeNew: AvlTree[StorageKey, StorageValue] =
//            tree.assembleTree(nodes.nodesList.map { n =>
//              val a = NodeSerilalizer.fromProto[StorageKey, StorageValue](n)
//              a
//            })
//          treeNew
//      }
//
//      val state = UtxoState(newTree1, Height @@ 0, settings.constants)
//
//      state.validateTreeAfterFastSync() shouldBe true
//    }
//    "check tree assembly(with incorrect nodes)" in {
//      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
//      val avl1: AvlTree[StorageKey, StorageValue] =
//        createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
//      val block1: Block = generateGenesisBlock(Height @@ 1)
//
//      val processor1: SnapshotProcessor =
//        snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
//      val manifest: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
//
//      val newTree: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](
//        manifest,
//        VLDBWrapper(
//          VersionalLevelDBCompanion(LevelDbFactory.factory.open(tmpDir, new Options), settings.levelDB, keySize = 32)
//        )
//      )
//
//      val newTree1: AvlTree[StorageKey, StorageValue] = manifest.chunksKeys.foldLeft(newTree) {
//        case (tree, key) =>
//          val chunk: SnapshotChunkMessage         = processor1.getChunkById(key).get
//          val nodes: SnapshotHolder.SnapshotChunk = SnapshotChunkSerializer.fromProto(chunk).get
//          val treeNew: AvlTree[StorageKey, StorageValue] =
//            tree.assembleTree(nodes.nodesList.map { n =>
//              val a = NodeSerilalizer.fromProto[StorageKey, StorageValue](n)
//              a match {
//                case l@InternalNode(_, _, _, _, _, _) =>
//                  l.copy(height = random.nextInt())
//                case l => l
//              }
//            })
//          treeNew
//      }
//
//      val state = UtxoState(newTree1, Height @@ 0, settings.constants)
//
//      Either.catchNonFatal(state.validateTreeAfterFastSync()).isLeft shouldBe true
//    }
//    "check chunk validity" in {}
//  }

  def createAvl(address: String, from: Int, to: Int): AvlTree[StorageKey, StorageValue] = {
    val firstDir: File = FileHelper.getRandomTempDir
    val firstStorage: VLDBWrapper = {
      VLDBWrapper(
        VersionalLevelDBCompanion(LevelDbFactory.factory.open(firstDir, new Options), settings.levelDB, keySize = 32)
      )
    }
    val boxes: IndexedSeq[(StorageKey, StorageValue)] = (from until to)
      .map(i => genAssetBox(address, i, nonce = i))
      .map { bx =>
        (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)
      }

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
