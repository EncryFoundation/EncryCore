package encry.modifiers.mempool.directive

import encry.local.TestHelper
import org.scalatest.FunSuite
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random

class AddPubKeyInfoDirectiveSerializerTest extends FunSuite {

  test("testParseBytes & testToBytes") {

    val pubKeyBytes = PublicKey @@ Random.randomBytes()
    val pubKeyProofBytes = Signature @@ Random.randomBytes(64)
    val pubKeyInfoBytes = Random.randomBytes(40)
    val pubKeyTypeId = 99.toByte

    val dir = AddPubKeyInfoDirective(TestHelper.Props.recipientAddr, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId, 0)

    val directiveSerialized = AddPubKeyInfoDirectiveSerializer.toBytes(dir)

    val directiveDeserialized = AddPubKeyInfoDirectiveSerializer.parseBytes(directiveSerialized)

    assert(directiveDeserialized.isSuccess, "Deserialization failed")

    assert(dir.bytes sameElements directiveDeserialized.get.bytes)
  }
}
