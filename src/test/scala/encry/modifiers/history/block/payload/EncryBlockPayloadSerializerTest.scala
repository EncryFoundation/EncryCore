package encry.modifiers.history.block.payload

import encry.modifiers.mempool.TransactionFactory
import encry.utils.TestHelper
import org.scalatest.FunSuite
import encry.ModifierId
import encry.view.history.Height

class EncryBlockPayloadSerializerTest extends FunSuite {

  test("testParseBytes & testToBytes") {

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath).slice(0, 10)

    val timestamp = 12345678L

    val txs = keys.map { k =>
      val useBoxes = IndexedSeq(factory.genAssetBox(k.publicImage.address))
      TransactionFactory.coinbaseTransactionScratch(k.publicImage, timestamp, useBoxes, 0, Height @@ 100)
    }

    val blockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val blockPayloadSerialized = EncryBlockPayloadSerializer.toBytes(blockPayload)

    val blockPayloadDeserialized = EncryBlockPayloadSerializer.parseBytes(blockPayloadSerialized)

    assert(blockPayloadDeserialized.isSuccess, "Deserialization failed.")

    assert(blockPayload.id sameElements blockPayloadDeserialized.get.id, "Payload id mismatch.")

    assert(txs.size == blockPayloadDeserialized.get.transactions.size, "Transactions quantity mismatch.")
  }
}
