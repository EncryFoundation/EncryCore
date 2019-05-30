package encry.modifiers.history

import encry.modifiers.mempool.{TransactionFactory}
import encry.utils.TestHelper
import org.encryfoundation.common.modifiers.history.{Payload, PayloadSerializer}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId}
import org.scalatest.FunSuite

class PayloadSerializerTest extends FunSuite {

  test("testParseBytes & testToBytes") {

    val factory = TestHelper
    val keys = factory.genKeys(10)

    val timestamp = 12345678L

    val txs: Seq[Transaction] = keys.map { k =>
      TransactionFactory.coinbaseTransactionScratch(k.publicImage, timestamp, 10L, 0, Height @@ 100)
    }

    val blockPayload = Payload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val blockPayloadSerialized = PayloadSerializer.toBytes(blockPayload)

    val blockPayloadDeserialized = PayloadSerializer.parseBytes(blockPayloadSerialized)

    assert(blockPayloadDeserialized.isSuccess, "Deserialization failed.")

    assert(blockPayload.id sameElements blockPayloadDeserialized.get.id, "Payload id mismatch.")

    assert(txs.size == blockPayloadDeserialized.get.txs.size, "Transactions quantity mismatch.")
  }
}
