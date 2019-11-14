package encry.view.fast.sync

import SnapshotChunkProto.SnapshotChunkMessage
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import encry.view.fast.sync.SnapshotHolder.SnapshotChunkSerializer
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import encry.view.fast.sync.FastSyncTestsUtils._

import scala.util.Try

class SnapshotChunkSerializerTest extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {

  "SnapshotChunkSerializer" should {
    "serialize|deserialize correctly" in {
      val (_, processor, _, _, manifest, _) = initializeTestState()

      val chunk: SnapshotChunkMessage                = processor.getChunkById(manifest.chunksKeys.head).get
      val chunkR: SnapshotHolder.SnapshotChunk       = SnapshotChunkSerializer.fromProto(chunk).get

      val bytesChunk: Array[Byte] = SnapshotChunkSerializer.toProto(chunkR).toByteArray
      bytesChunk.isEmpty shouldBe false
      val ser: SnapshotChunkMessage           = Try(SnapshotChunkMessage.parseFrom(bytesChunk)).get
      val deser: SnapshotHolder.SnapshotChunk = SnapshotChunkSerializer.fromProto(ser).get

      chunkR.id.sameElements(deser.id) shouldBe true
    }
  }

}
