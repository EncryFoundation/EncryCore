package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.FunSuite

class TransactionSerializerTest extends FunSuite with InstanceFactory {

  test("toBytes & parseBytes (Transfer)") {

    val tx = paymentTransactionDynamic

    val txSerialized = tx.bytes

    val txDeserialized = TransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.id sameElements txDeserialized.get.id, "Id mismatch.")
  }

  test("toBytes & parseBytes (Coinbase)") {

    val tx = coinbaseTransaction

    val txSerialized = tx.bytes

    val txDeserialized = TransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.id sameElements txDeserialized.get.id, "Id mismatch.")
  }
}
