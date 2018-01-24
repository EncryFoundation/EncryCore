package encry.modifiers.history.block.header

import encry.consensus.Difficulty
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random


class EncryBlockHeaderSerializerTest extends FunSuite {

  test("testToBytes & testParseBytes") {

    val blockHeader = EncryBlockHeader(
      99: Byte,
      new PublicKey25519Proposition(PublicKey @@ Random.randomBytes()),
      Signature25519(Signature @@ Random.randomBytes(64)),
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(33),
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
