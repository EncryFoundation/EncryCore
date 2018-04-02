package encry.modifiers.history.block.header

import encry.utils.EncryGenerator
import org.scalatest.FunSuite


class EncryBlockHeaderSerializerTest extends FunSuite with EncryGenerator{

  test("testToBytes & testParseBytes") {

    val blockHeader = genHeader

    val blockHeaderSerialized = EncryBlockHeaderSerializer.toBytes(blockHeader)

    val blockHeaderDeserialized = EncryBlockHeaderSerializer.parseBytes(blockHeaderSerialized)

    assert(blockHeaderDeserialized.isSuccess, "Deserialization failed.")

    assert(blockHeader.id sameElements blockHeaderDeserialized.get.id, "HeaderId mismatch.")
  }
}
