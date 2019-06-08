package encry.modifiers.mempool

import org.encryfoundation.common.modifiers.mempool.transaction.{Proof, ProofSerializer}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue._
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.Random

class ProofSerializerSpec extends PropSpec with Matchers {

  property("Serialize and parse proof with IntValue") {
    val value = IntValue(1000)
    val proof = Proof(value, Some("IntValue"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

  property("Serialize and parse proof with ByteValue") {
    val value = ByteValue(7)
    val proof = Proof(value, Some("ByteValue"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

  property("Serialize and parse proof with BoolValue") {
    val value = BoolValue(true)
    val proof = Proof(value, Some("BoolValue"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

  property("Serialize and parse proof with StringValue") {
    val value = StringValue("somerandomstringwithstrangesymbols!@#$%^&*()\n_+~/")
    val proof = Proof(value, Some("StringValue"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

  property("Serialize and parse proof with ByteCollectionValue") {
    val value = ByteCollectionValue(Random.randomBytes().toList)
    val proof = Proof(value, Some("ByteCollectionValue"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

  property("Serialize and parse proof with Signature25519Value") {
    val value = Signature25519Value(Random.randomBytes().toList)
    val proof = Proof(value, Some("Signature25519Value"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

  property("Serialize and parse proof with MultiSignatureValue") {
    val value = MultiSignatureValue((1 to 5).map(_ => Random.randomBytes().toList).toList)
    val proof = Proof(value, Some("MultiSignatureValue"))

    val bytes = proof.bytes

    val parsed = ProofSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    parsed.get shouldEqual proof
  }

}
