package encry.view.fastSync

import java.io.File
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.SnapshotChunkSerializer
import encry.view.state.avlTree.{AvlTree, NodeSerilalizer}
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.utils.TaggedTypes.Height
import scorex.utils.Random

class SubtreesAssemblerTest extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  "Subtrees assembler" should {
    "assemble tree correctly" in {
      val avlTree = createAvl
      val root = avlTree.rootNode
      val block = generateGenesisBlock(Height @@ 1)
      val (manifest, chunks) = avlTree.initializeSnapshotData(block)
      val serializedChunks = chunks.map(SnapshotChunkSerializer.toProto)
      val deserializedChunks = serializedChunks.map(SnapshotChunkSerializer.fromProto(_).get)

      val firstDir: File = FileHelper.getRandomTempDir
      val firstStorage: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val avlTree2 = AvlTree[StorageKey, StorageValue](manifest, firstStorage)

      avlTree2.find(root.key).isDefined shouldBe true

      deserializedChunks.foldLeft(avlTree2) { case (avl, chunk) =>
        val newAvl: AvlTree[StorageKey, StorageValue] = avl.assembleTree(avl, List(chunk))
        chunk.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).forall { node =>
          newAvl.find(node.key).isDefined
        } shouldBe true
        newAvl
      }

      val firstDir1: File = FileHelper.getRandomTempDir
      val firstStorage1: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir1, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val avlTree3 = AvlTree[StorageKey, StorageValue](manifest, firstStorage1)
      val avlTree4 = avlTree3.assembleTree(avlTree3, deserializedChunks)

      deserializedChunks.foreach(ch =>
        ch.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).forall { node =>
          avlTree4.find(node.key).isDefined
        } shouldBe true
      )
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
}