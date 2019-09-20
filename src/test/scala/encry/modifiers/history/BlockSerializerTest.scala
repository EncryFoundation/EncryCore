package encry.modifiers.history

import encry.modifiers.mempool.TransactionFactory
import encry.settings.Settings
import encry.utils.{EncryGenerator, TestHelper}
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.scalatest.FunSuite
import scorex.crypto.hash.Digest32
import scorex.utils.Random

class BlockSerializerTest extends FunSuite with EncryGenerator with Settings {

  test("testToBytes $ testFromBytes") {

    val blockHeader = Header(
      99: Byte,
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      99999L,
      199,
      999L,
      settings.constants.InitialDifficulty,
      EquihashSolution(Seq(1, 2, 3)),
      Array.emptyByteArray
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

    val block = Block(blockHeader,blockPayload)

    val blockSererialized = BlockSerializer.toBytes(block)

    val blockDeserealized = BlockSerializer.parseBytes(blockSererialized).get

    assert(Algos.hash(block.bytes) sameElements Algos.hash(blockDeserealized.bytes), "Block bytes mismatch.")
  }
}
