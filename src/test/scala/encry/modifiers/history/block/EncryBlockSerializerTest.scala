package encry.modifiers.history.block

import encry.account.Address
import encry.consensus.Difficulty
import encry.local.TestHelper
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.PaymentTransaction
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random

class EncryBlockSerializerTest extends FunSuite {

  test("testToBytes $ testFromBytes") {

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

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath).slice(0, 10)

    val txs = keys.map { key =>
      val proposition = key.publicImage
      val fee = factory.Props.txFee
      val timestamp = 12345678L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, factory.Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        key,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val blockPayload = new EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val adProofs = ADProofs(ModifierId @@ Random.randomBytes(), SerializedAdProof @@ Random.randomBytes())

    val eB = new EncryBlock(blockHeader,blockPayload,Option(adProofs))

    val blockSer = EncryBlockSerializer.toBytes(eB)

    val blockDeSer = EncryBlockSerializer.parseBytes(blockSer).get

    assert(eB.adProofsOpt.get.bytes sameElements blockDeSer.adProofsOpt.get.bytes,"ADProofs bytes mismatch.")
  }
}
