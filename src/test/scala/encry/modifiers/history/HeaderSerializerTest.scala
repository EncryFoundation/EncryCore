package encry.modifiers.history

import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

class HeaderSerializerTest extends PropSpec with Matchers with EncryGenerator{

  property("testToBytes & testParseBytes") {

    val blockHeader = genHeader

    val blockHeaderSerialized = HeaderSerializer.toBytes(blockHeader)

    val blockHeaderDeserialized = HeaderSerializer.parseBytes(blockHeaderSerialized)

    blockHeaderDeserialized.isSuccess shouldBe true

    blockHeader.id shouldEqual blockHeaderDeserialized.get.id

    blockHeader.difficulty shouldEqual blockHeaderDeserialized.get.difficulty
  }
}
