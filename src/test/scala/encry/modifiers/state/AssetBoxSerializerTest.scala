package encry.modifiers.state

import encry.modifiers.InstanceFactory
import encry.modifiers.state.box.AssetBoxSerializer
import org.scalatest.FunSuite

class AssetBoxSerializerTest extends FunSuite with InstanceFactory {

  test("toBytes & parseBytes") {

    val bx = assetBox

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }

  test("toBytes & parseBytes (OpenProposition)") {

    val bx = openAssetBox

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }
}
