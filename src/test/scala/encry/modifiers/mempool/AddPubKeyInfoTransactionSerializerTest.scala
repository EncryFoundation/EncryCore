package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.FunSuite

class AddPubKeyInfoTransactionSerializerTest extends FunSuite {

  test("toBytes & parseBytes") {

    val tx = InstanceFactory.addPubKeyInfoTransaction

    val txSerialized = tx.bytes

    val txDeserialized = AddPubKeyInfoTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }
}
