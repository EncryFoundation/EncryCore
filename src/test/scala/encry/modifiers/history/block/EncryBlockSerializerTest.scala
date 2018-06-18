package encry.modifiers.history.block

import encry.crypto.PublicKey25519
import encry.crypto.equihash.EquihashSolution
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.TransactionFactory
import encry.modifiers.state.box.proof.Signature25519
import encry.settings.Constants
import encry.utils.TestHelper
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}
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
      Constants.Chain.InitialDifficulty,
      EquihashSolution(Seq(1, 2, 3))
    )

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath).slice(0, 10)

    val fee = factory.Props.txFee
    val timestamp = 12345678L

    val txs = keys.map { k =>
      val useBoxes = IndexedSeq(factory.genAssetBox(k.publicImage.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, factory.Props.recipientAddr, factory.Props.boxValue)
    }

    val blockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val adProofs = ADProofs(ModifierId @@ Random.randomBytes(), SerializedAdProof @@ Random.randomBytes())

    val block = EncryBlock(blockHeader,blockPayload,Option(adProofs))

    val blockSererialized = EncryBlockSerializer.toBytes(block)

    val blockDeserealized = EncryBlockSerializer.parseBytes(blockSererialized).get

    assert(block.adProofsOpt.get.bytes sameElements blockDeserealized.adProofsOpt.get.bytes,"ADProofs bytes mismatch.")

    assert(block.id sameElements blockDeserealized.id, "Block Id mismatch.")
  }
}
