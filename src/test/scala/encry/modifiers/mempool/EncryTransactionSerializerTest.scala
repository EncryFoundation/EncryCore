package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.FunSuite

class EncryTransactionSerializerTest extends FunSuite with InstanceFactory {

  test("toBytes & parseBytes (Transafer)") {

    val tx = paymentTransactionDynamic

    val txSerialized = tx.bytes

    val txDeserialized = EncryTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }

  test("toBytes & parseBytes (Coinbase)") {

    val tx = coinbaseTransaction

    val txSerialized = tx.bytes

    val txDeserialized = EncryTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }

  test("toBytes & parseBytes (AddPubKey)") {

    val tx = addPubKeyInfoTransaction

    val txSerialized = tx.bytes

    val txDeserialized = EncryTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }
}
