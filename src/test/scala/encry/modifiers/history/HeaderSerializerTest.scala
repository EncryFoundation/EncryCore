package encry.modifiers.history

import org.encryfoundation.common.modifiers.history.HeaderSerializer
import org.scalatest.{Matchers, PropSpec}
import encry.utils.TestEntityGenerator.genHeader

class HeaderSerializerTest extends PropSpec with Matchers {

  property("testToBytes & testParseBytes") {

    val blockHeader = genHeader

    val blockHeaderSerialized = HeaderSerializer.toBytes(blockHeader)

    val blockHeaderDeserialized = HeaderSerializer.parseBytes(blockHeaderSerialized)

    blockHeaderDeserialized.isSuccess shouldBe true

    blockHeader.id shouldEqual blockHeaderDeserialized.get.id

    blockHeader.difficulty shouldEqual blockHeaderDeserialized.get.difficulty
  }
}
