package encry.modifiers.history.block.payload

import encry.account.Address
import encry.crypto.PublicKey25519
import encry.local.TestHelper
import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.PaymentTransaction
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.state.box.proof.Signature25519
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.crypto.signatures.Curve25519

class EncryBlockPayloadSerializerTest extends FunSuite {

  test("testParseBytes & testToBytes") {

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath).slice(0, 10)

    val txs = keys.map { key =>
      val pubKey = PublicKey25519(key.publicKeyBytes)
      val fee = factory.Props.txFee
      val timestamp = 12335467L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq(
        TransferDirective(factory.Props.recipientAddr, factory.Props.boxValue, 1),
        TransferDirective(factory.Props.recipientAddr, factory.Props.boxValue, 2)
      )
      val sig = Signature25519(Curve25519.sign(
        key.privKeyBytes,
        PaymentTransaction.getMessageToSign(pubKey, fee, timestamp, useBoxes, outputs))
      )
      PaymentTransaction(pubKey, fee, timestamp, sig, useBoxes, outputs)
    } :+ InstanceFactory.addPubKeyInfoTransaction() :+ InstanceFactory.coinbaseTransaction

    val blockPayload = new EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), txs)

    val blockPayloadSerialized = EncryBlockPayloadSerializer.toBytes(blockPayload)

    val blockPayloadDeserialized = EncryBlockPayloadSerializer.parseBytes(blockPayloadSerialized)

    assert(blockPayloadDeserialized.isSuccess, "Deserialization failed.")

    assert(blockPayload.id sameElements blockPayloadDeserialized.get.id, "Payload id mismatch.")

    assert(txs.size == blockPayloadDeserialized.get.transactions.size, "Transactions quantity mismatch.")
  }
}
