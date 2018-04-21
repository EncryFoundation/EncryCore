package encry.modifiers.state

import encry.modifiers.InstanceFactory
import encry.modifiers.state.box.AssetCreationBoxSerializer
import org.scalatest.FunSuite

class AssetCreationBoxSerializerTest extends FunSuite with InstanceFactory {

  test("toBytes & parseBytes") {

    val bx = AssetCreationBoxI

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetCreationBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }
}
