package encry.modifiers.mempool

import encry.crypto.Address
import encry.local.TestHelper
import org.scalatest.FunSuite
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.NetworkTime
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58

class PaymentTransactionSerializerTest extends FunSuite {

  test("toBytes & parseBytes") {

    val factory = TestHelper
    val key = factory.getOrGenerateKeys(factory.Props.keysFilePath).head

    val tx = {
      val proposition = key.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, factory.Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        key,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val txSerialized = PaymentTransactionSerializer.toBytes(tx)

    val txDeserialized = PaymentTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")
    assert(tx.txHash sameElements txDeserialized.get.txHash, "Id mismatch.")
  }

}
