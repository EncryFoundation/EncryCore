package encry.modifiers.history.block

import encry.consensus.Difficulty
import encry.crypto.Address
import encry.local.TestHelper
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.PaymentTransaction
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.NetworkTime
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.utils.Random

class EncryBlockSerializerTest extends FunSuite {

  test("testToBytes $ testFromBytes") {

    val blockHeader = EncryBlockHeader(
        99: Byte,
        ModifierId @@ Random.randomBytes(),
        Digest32 @@ Random.randomBytes(),
        ADDigest @@ Random.randomBytes(),
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
      val timestamp = NetworkTime.time()
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
