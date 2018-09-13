package encry.modifiers.history.block.payload

import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.TestHelper
import org.scalatest.FunSuite
import encry.view.history.History.Height

class EncryBlockPayloadSerializerTest extends FunSuite {

  test("testParseBytes & testToBytes") {

    val factory = TestHelper
    val keys = factory.genKeys(10)

    val timestamp = 12345678L

    val txs: Seq[Transaction] = keys.map { k =>
      TransactionFactory.coinbaseTransactionScratch(k.publicImage, timestamp, 10L, 0, Height @@ 100)
    }

    val blockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val blockPayloadSerialized = EncryBlockPayloadSerializer.toBytes(blockPayload)

    val blockPayloadDeserialized = EncryBlockPayloadSerializer.parseBytes(blockPayloadSerialized)

    assert(blockPayloadDeserialized.isSuccess, "Deserialization failed.")

    assert(blockPayload.id sameElements blockPayloadDeserialized.get.id, "Payload id mismatch.")

    assert(txs.size == blockPayloadDeserialized.get.transactions.size, "Transactions quantity mismatch.")
  }
}
