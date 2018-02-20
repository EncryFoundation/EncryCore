package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.FunSuite

class PaymentTransactionSerializerTest extends FunSuite {

  test("toBytes & parseBytes") {

    val tx = InstanceFactory.paymentTransactionValid()

    val txSerialized = tx.bytes

    val txDeserialized = PaymentTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }
}
