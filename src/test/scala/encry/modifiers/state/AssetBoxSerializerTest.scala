package encry.modifiers.state

import encry.modifiers.InstanceFactory
import org.encryfoundation.common.modifiers.state.box.AssetBoxSerializer
import org.scalatest.FunSuite

class AssetBoxSerializerTest extends FunSuite with InstanceFactory {

  test("toBytes & parseBytes") {

    val bx = AssetBoxI

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }

  test("toBytes & parseBytes (OpenProposition)") {

    val bx = OpenAssetBoxI

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }
}
