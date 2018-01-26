package encry.modifiers.mempool

import encry.account.Address
import encry.local.TestHelper
import encry.view.history.Height
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random

object InstanceFactory {

  private val genHelper = TestHelper
  private val key = genHelper.getOrGenerateKeys(genHelper.Props.keysFilePath).head

  val paymentTransactionValid: PaymentTransaction = {
    val proposition = key.publicImage
    val fee = genHelper.Props.txFee
    val timestamp = 1234567L
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val outputs = IndexedSeq((Address @@ genHelper.Props.recipientAddr, genHelper.Props.boxValue))
    val sig = PrivateKey25519Companion.sign(
      key,
      PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
    )
    PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
  }

  val paymentTransactionInvalid: PaymentTransaction = {
    val proposition = key.publicImage
    val fee = genHelper.Props.txFee
    val timestamp = 1234567L
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val outputs = IndexedSeq((Address @@ genHelper.Props.recipientAddr, genHelper.Props.boxValue))
    val sig = Signature25519(Signature @@ Random.randomBytes(64))
    PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
  }

  val coinbaseTransaction = CoinbaseTransaction(
    PublicKey25519Proposition(PublicKey @@ Random.randomBytes()),
    178999L,
    Signature25519(Signature @@ Random.randomBytes(64)),
    IndexedSeq(ADKey @@ Random.randomBytes(), ADKey @@ Random.randomBytes()),
    999L,
    Height @@ 0
  )
}
