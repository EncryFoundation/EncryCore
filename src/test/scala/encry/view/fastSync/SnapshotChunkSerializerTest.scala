//package encry.view.fastSync
//
//import java.io.File
//import SnapshotChunkProto.SnapshotChunkMessage
//import encry.view.state.avlTree.utils.implicits.Instances._
//import encry.modifiers.InstanceFactory
//import encry.settings.TestNetSettings
//import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
//import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
//import encry.utils.FileHelper
//import encry.view.fastSync.SnapshotHolder.SnapshotChunkSerializer
//import encry.view.state.UtxoState
//import encry.view.state.avlTree.AvlTree
//import org.encryfoundation.common.utils.TaggedTypes.Height
//import org.iq80.leveldb.Options
//import org.scalatest.{ Matchers, OneInstancePerTest, WordSpecLike }
//import scorex.utils.Random
//import scala.util.Try
//
//class SnapshotChunkSerializerTest
//    extends WordSpecLike
//    with Matchers
//    with InstanceFactory
//    with OneInstancePerTest
//    with TestNetSettings {
//
//  "SnapshotChunkSerializer" should {
//    "serialize|deserialize correctly" in {
//      val avl1                                 = createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia")
//      val block1                               = generateGenesisBlock(Height @@ 1)
//      val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
//      val processor1: SnapshotProcessor =
//        snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)
//      val manifest1: SnapshotHolder.SnapshotManifest = processor1.bestPotentialManifest.get
//      val chunk: SnapshotChunkMessage                = processor1.getChunkById(manifest1.chunksKeys.head).get
//      val chunkR: SnapshotHolder.SnapshotChunk       = SnapshotChunkSerializer.fromProto(chunk).get
//
//      val bytesChunk: Array[Byte] = SnapshotChunkSerializer.toProto(chunkR).toByteArray
//      bytesChunk.isEmpty shouldBe false
//      val ser: SnapshotChunkMessage           = Try(SnapshotChunkMessage.parseFrom(bytesChunk)).get
//      val deser: SnapshotHolder.SnapshotChunk = SnapshotChunkSerializer.fromProto(ser).get
//
//      chunkR.id.sameElements(deser.id) shouldBe true
//    }
//  }
//
//}
