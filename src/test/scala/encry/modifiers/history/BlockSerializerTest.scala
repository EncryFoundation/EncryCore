package encry.modifiers.history

import encry.modifiers.mempool.TransactionFactory
import encry.settings.Constants
import encry.utils.{EncryGenerator, TestHelper}
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, SerializedAdProof}
import org.scalatest.FunSuite
import scorex.crypto.hash.Digest32
import scorex.utils.Random

class BlockSerializerTest extends FunSuite with EncryGenerator {

  test("testToBytes $ testFromBytes") {

    val blockHeader = Header(
      99: Byte,
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
    val keys = factory.genKeys(10)

    val fee = factory.Props.txFee
    val timestamp = 12345678L

    val txs = keys.map { k =>
      val useBoxes = IndexedSeq(factory.genAssetBox(k.publicImage.address.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, fee,
        timestamp, useBoxes, randomAddress, factory.Props.boxValue)
    }

    val blockPayload = Payload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val adProofs = ADProofs(ModifierId @@ Random.randomBytes(), SerializedAdProof @@ Random.randomBytes())

    val block = Block(blockHeader,blockPayload,Option(adProofs))

    val blockSererialized = BlockSerializer.toBytes(block)

    val blockDeserealized = BlockSerializer.parseBytes(blockSererialized).get

    assert(block.adProofsOpt.get.bytes sameElements blockDeserealized.adProofsOpt.get.bytes,"ADProofs bytes mismatch.")

    assert(Algos.hash(block.bytes) sameElements Algos.hash(blockDeserealized.bytes), "Block bytes mismatch.")
  }
}
