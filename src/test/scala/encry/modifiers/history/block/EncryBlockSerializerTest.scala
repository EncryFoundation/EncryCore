package encry.modifiers.history.block

import encry.account.Address
import encry.consensus.Difficulty
import encry.crypto.{PublicKey25519, Signature25519}
import encry.local.TestHelper
import encry.modifiers.InstanceFactory
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.PaymentTransaction
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.utils.Random

class EncryBlockSerializerTest extends FunSuite {

  test("testToBytes $ testFromBytes") {

    val blockHeader = EncryBlockHeader(
      99: Byte,
      PublicKey25519(PublicKey @@ Random.randomBytes()),
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
      val pubKey = PublicKey25519(key.publicImage.pubKeyBytes)
      val fee = factory.Props.txFee
      val timestamp = 12345678L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, factory.Props.boxValue))
      val sig = Signature25519(Curve25519.sign(
        key.privKeyBytes,
        PaymentTransaction.getMessageToSign(pubKey, fee, timestamp, useBoxes, outputs)
      ))
      PaymentTransaction(pubKey, fee, timestamp, sig, useBoxes, outputs)
    } :+ InstanceFactory.addPubKeyInfoTransaction() :+ InstanceFactory.coinbaseTransaction

    val blockPayload = new EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val adProofs = ADProofs(ModifierId @@ Random.randomBytes(), SerializedAdProof @@ Random.randomBytes())

    val block = new EncryBlock(blockHeader,blockPayload,Option(adProofs))

    val blockSererialized = EncryBlockSerializer.toBytes(block)

    val blockDeserealized = EncryBlockSerializer.parseBytes(blockSererialized).get

    assert(block.adProofsOpt.get.bytes sameElements blockDeserealized.adProofsOpt.get.bytes,"ADProofs bytes mismatch.")

    assert(block.id sameElements blockDeserealized.id, "Block Id mismatch.")
  }
}
