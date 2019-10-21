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
import encry.view.state.avlTree.{AvlTree, EmptyNode, InternalNode, LeafNode, Node, NodeSerilalizer, ShadowNode}
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.TaggedTypes.Height
import scorex.utils.Random

class SubtreesAssemblerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings
    with StrictLogging {

  "Trees validator" should {
    "check tree assembly" in {
      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
      val avl1: AvlTree[StorageKey, StorageValue] =
        createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 1000)
      val block1: Block = generateGenesisBlock(Height @@ 1)

      val processor1: SnapshotProcessor =
        snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
      val manifest: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get

      val newTree: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](
        manifest,
        VLDBWrapper(
          VersionalLevelDBCompanion(LevelDbFactory.factory.open(tmpDir, new Options), settings.levelDB, keySize = 32)
        )
      )

      val newTree1: AvlTree[StorageKey, StorageValue] = manifest.chunksKeys.foldLeft(newTree) {
        case (tree, key) =>
          val chunk: SnapshotChunkMessage         = processor1.getChunkById(key).get
          val nodes: SnapshotHolder.SnapshotChunk = SnapshotChunkSerializer.fromProto(chunk).get
          val treeNew: AvlTree[StorageKey, StorageValue] =
            tree.assembleTree(nodes.nodesList.map { n =>
              val a = NodeSerilalizer.fromProto[StorageKey, StorageValue](n)
              a
            })
          treeNew
      }

      val state = UtxoState(newTree1, Height @@ 0, settings.constants)

      state.validateTreeAfterFastSync() shouldBe true
    }
    "check chunk validity" in {

    }
//    "assemble tree correctly" in {
//      val avlTree: AvlTree[StorageKey, StorageValue] = createAvl
//      val root: Node[StorageKey, StorageValue]       = avlTree.rootNode
//      val block: Block                               = generateGenesisBlock(Height @@ 1)
//      val (manifest, chunks)                         = avlTree.initializeSnapshotData(block)
//      val serializedChunks                           = chunks.map(SnapshotChunkSerializer.toProto)
//      val deserializedChunks                         = serializedChunks.map(SnapshotChunkSerializer.fromProto(_).get)
//
//      val firstDir: File = FileHelper.getRandomTempDir
//      val firstStorage: VLDBWrapper = {
//        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
//        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
//      }
//      val avlTree2 = AvlTree[StorageKey, StorageValue](manifest, firstStorage)
//
//      avlTree2.find(root.key).isDefined shouldBe true
//
//      deserializedChunks.foldLeft(avlTree2) {
//        case (avl, chunk) =>
//          val newAvl: AvlTree[StorageKey, StorageValue] =
//            avl.assembleTree(chunk.nodesList.map(l => NodeSerilalizer.fromProto[StorageKey, StorageValue](l)))
//          chunk.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).forall { node =>
//            newAvl.find(node.key).isDefined
//          } shouldBe true
//          newAvl
//      }
//
//      val firstDir1: File = FileHelper.getRandomTempDir
//      val firstStorage1: VLDBWrapper = {
//        val levelDBInit = LevelDbFactory.factory.open(firstDir1, new Options)
//        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
//      }
//      val avlTree3 = AvlTree[StorageKey, StorageValue](manifest, firstStorage1)
//      val avlTree4 = avlTree3.assembleTree(
//        deserializedChunks.flatMap(_.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)))
//      )
//
//      deserializedChunks.foreach(
//        ch =>
//          ch.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).forall { node =>
//            avlTree4.find(node.key).isDefined
//          } shouldBe true
//      )
//    }
  }

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
