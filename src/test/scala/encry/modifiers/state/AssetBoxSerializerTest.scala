package encry.modifiers.state

import encry.utils.Keys
import org.encryfoundation.common.modifiers.state.box.{AssetBox, AssetBoxSerializer, EncryProposition}
import org.scalatest.FunSuite

class AssetBoxSerializerTest extends FunSuite with Keys {

  lazy val assetBoxI: AssetBox = AssetBox(EncryProposition.pubKeyLocked(publicKey.pubKeyBytes), 999L, 100000L)
  lazy val openAssetBoxI: AssetBox = AssetBox(EncryProposition.open, 999L, 100000L)

  test("toBytes & parseBytes") {

    val bx = assetBoxI

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }

  test("toBytes & parseBytes (OpenProposition)") {

    val bx = openAssetBoxI

    val bxSerialized = bx.bytes

    val bxDeserialized = AssetBoxSerializer.parseBytes(bxSerialized)

    assert(bxDeserialized.isSuccess, "Deserialization failed.")

    assert(bx.bytes sameElements bxDeserialized.get.bytes, "Id mismatch.")
  }
}
