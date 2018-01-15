package encry.modifiers.history.block.header

import encry.consensus.Difficulty
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.utils.Random


class EncryBlockHeaderSerializerTest extends FunSuite {

  test("testToBytes & testParseBytes") {

    val blockHeader = EncryBlockHeader(
      99: Byte,
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      99999L,
      199,
      999L,
      Difficulty @@ BigInt(999999999999999L)
    )

    val blockHeaderSerialized = EncryBlockHeaderSerializer.toBytes(blockHeader)

    val blockHeaderDeserialized = EncryBlockHeaderSerializer.parseBytes(blockHeaderSerialized)

    assert(blockHeaderDeserialized.isSuccess, "Deserialization failed.")

    assert(blockHeader.id sameElements blockHeaderDeserialized.get.id, "HeaderId mismatch.")
  }

}
