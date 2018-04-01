package encry.modifiers.state

import encry.modifiers.InstanceFactory
import encry.modifiers.state.box.AssetBoxSerializer
import org.scalatest.FunSuite

class AssetBoxSerializerTest extends FunSuite {

  test("toBytes & parseBytes") {

    val bx = InstanceFactory.assetBox

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }

  test("toBytes & parseBytes (OpenProposition)") {

    val bx = InstanceFactory.openAssetBox

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }
}
