package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.FunSuite

class EncryTransactionSerializerTest extends FunSuite {

  test("toBytes & parseBytes") {

    val tx = InstanceFactory.paymentTransactionValid

    val txSerialized = tx.bytes

    val txDeserialized = EncryTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }
}
