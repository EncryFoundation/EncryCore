package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.{SnapshotChunk, SnapshotManifest}
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

  "Subtrees" should {
    "assembly correctly by small chunks" in {
      val firstDir: File = FileHelper.getRandomTempDir
      val firstStorage: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
      val interval: Int = 20
      val boxes = (0 to interval).map { i =>
        val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
        genAssetBox(addr, i, nonce = i)
      }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

      val newAvl: AvlTree[StorageKey, StorageValue] = firstAvl.insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(),
        boxes.toList,
        List.empty
      )
      val rootKey = newAvl.rootNode.key
      val dummyBlock = generateGenesisBlock(Height @@ 1)
      val newInfo: (SnapshotManifest, List[SnapshotChunk]) = newAvl.initializeSnapshotData(dummyBlock)
      newInfo._2.nonEmpty shouldBe true
      val serChunks: List[SnapshotChunkMessage] = newInfo._2.map(SnapshotHolder.SnapshotChunkSerializer.toProto)
      val deserChunks: List[SnapshotChunk] = serChunks.map(SnapshotHolder.SnapshotChunkSerializer.fromProto)
        .foldLeft(List.empty[SnapshotChunk]) {
          case (list, proto) => proto match {
            case Left(value) => list
            case Right(value) => value :: list
          }
        }
      val dir2 = FileHelper.getRandomTempDir
      val storage2 = {
        val levelDBInit = LevelDbFactory.factory.open(dir2, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val avlStorage2 = AvlTree[StorageKey, StorageValue](newInfo._1, storage2)
      avlStorage2.find(rootKey).isDefined shouldBe true
      deserChunks.foldLeft(avlStorage2) { case (avl, chunk) =>
        val newAvl: AvlTree[StorageKey, StorageValue] = avl.assembleTree(avl, List(chunk))
        chunk.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).forall { node =>
          val cond = newAvl.find(node.key).isDefined
          cond shouldBe true
          cond
        }
        newAvl
      }
    }
    "Assembly correct by one big chunk" in {
      val firstDir: File = FileHelper.getRandomTempDir
      val firstStorage: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
      val interval: Int = 20
      val boxes = (0 to interval).map { i =>
        val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
        genAssetBox(addr, i, nonce = i)
      }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

      val newAvl: AvlTree[StorageKey, StorageValue] = firstAvl.insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(),
        boxes.toList,
        List.empty
      )
      val rootKey = newAvl.rootNode.key
      val dummyBlock = generateGenesisBlock(Height @@ 1)
      val newInfo: (SnapshotManifest, List[SnapshotChunk]) = newAvl.initializeSnapshotData(dummyBlock)
      newInfo._2.nonEmpty shouldBe true
      val serChunks: List[SnapshotChunkMessage] = newInfo._2.map(SnapshotHolder.SnapshotChunkSerializer.toProto)
      val deserChunks: List[SnapshotChunk] = serChunks.map(SnapshotHolder.SnapshotChunkSerializer.fromProto)
        .foldLeft(List.empty[SnapshotChunk]) {
          case (list, proto) => proto match {
            case Left(value) => list
            case Right(value) => value :: list
          }
        }
      val dir2 = FileHelper.getRandomTempDir
      val storage2 = {
        val levelDBInit = LevelDbFactory.factory.open(dir2, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val avlStorage2 = AvlTree[StorageKey, StorageValue](newInfo._1, storage2)
      avlStorage2.find(rootKey).isDefined shouldBe true
      val newAvl1: AvlTree[StorageKey, StorageValue] = avlStorage2.assembleTree(avlStorage2, deserChunks)
      deserChunks.map(_.nodesList.map(NodeSerilalizer.fromProto[StorageKey, StorageValue](_)).forall { node =>
        val cond = newAvl1.find(node.key).isDefined
        cond shouldBe true
        cond
      })

    }
  }
}