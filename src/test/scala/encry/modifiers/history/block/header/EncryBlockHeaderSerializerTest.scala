package encry.modifiers.history.block.header

import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}


class EncryBlockHeaderSerializerTest extends PropSpec with Matchers with EncryGenerator{

  property("testToBytes & testParseBytes") {

    val blockHeader = genHeader

    val blockHeaderSerialized = EncryBlockHeaderSerializer.toBytes(blockHeader)

    val blockHeaderDeserialized = EncryBlockHeaderSerializer.parseBytes(blockHeaderSerialized)

    blockHeaderDeserialized.isSuccess shouldBe true

    blockHeader.id shouldEqual blockHeaderDeserialized.get.id

    blockHeader.nBits shouldEqual blockHeaderDeserialized.get.nBits
  }
}
