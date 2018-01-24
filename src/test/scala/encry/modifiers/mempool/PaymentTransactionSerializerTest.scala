package encry.modifiers.mempool

import encry.account.Address
import encry.local.TestHelper
import org.scalatest.FunSuite
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.NetworkTime
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58

class PaymentTransactionSerializerTest extends FunSuite {

  test("toBytes & parseBytes") {

    val tx = InstanceFactory.paymentTransactionValid

    val txSerialized = PaymentTransactionSerializer.toBytes(tx)

    val txDeserialized = PaymentTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }

}
