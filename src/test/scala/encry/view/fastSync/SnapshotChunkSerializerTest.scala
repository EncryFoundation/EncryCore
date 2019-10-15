package encry.view.fastSync

import java.io.File
import SnapshotChunkProto.SnapshotChunkMessage
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.SnapshotChunkSerializer
import encry.view.state.avlTree.AvlTree
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.Options
import org.scalatest.{ Matchers, OneInstancePerTest, WordSpecLike }
import scorex.utils.Random
import scala.util.Try

class SnapshotChunkSerializerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {

  "SnapshotChunkSerializer" should {
    "serialize|deserialize correctly" in {
//      val avl1                                = createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia")
//      val block1                              = generateGenesisBlock(Height @@ 1)
//      val chunk: SnapshotHolder.SnapshotChunk = avl1.initializeSnapshotData(block1)._2.head
//      val bytes: Array[Byte]                  = SnapshotChunkSerializer.toProto(chunk).toByteArray
//      val ser: SnapshotChunkMessage           = Try(SnapshotChunkMessage.parseFrom(bytes)).toOption.get
//      val deser: SnapshotHolder.SnapshotChunk = SnapshotChunkSerializer.fromProto(ser).get
//
//      chunk.id.sameElements(deser.id) shouldBe true
    }
  }

  def createAvl(address: String): AvlTree[StorageKey, StorageValue] = {
    val firstDir: File = FileHelper.getRandomTempDir
    val firstStorage: VLDBWrapper = {
      val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    val interval: Int = 10
    val boxes: IndexedSeq[(StorageKey, StorageValue)] = (0 to interval).map { i =>
      genAssetBox(address, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
    firstAvl.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )._1
  }

  def tmpDir: File = FileHelper.getRandomTempDir
}
