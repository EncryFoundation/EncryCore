package encry.modifiers.history

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.utils.EncryGenerator
import org.encryfoundation.common.crypto.equihash.{EquihashSolution, EquihashSolutionsSerializer}
import org.encryfoundation.common.modifiers.history.{Header, HeaderSerializer}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Digest32

import scala.util.Try

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
