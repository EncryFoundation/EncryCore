package encry.modifiers

import encry.account.Address
import encry.local.TestHelper
import encry.modifiers.mempool.{AddPubKeyInfoTransaction, CoinbaseTransaction, PaymentTransaction}
import encry.modifiers.state.box.PubKeyInfoBox
import encry.modifiers.state.box.proposition.AddressProposition
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
  private val timestamp = System.currentTimeMillis()

  val paymentTransactionValid: PaymentTransaction = {
    val proposition = key.publicImage
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address),
      genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val outputs = IndexedSeq((Address @@ genHelper.Props.recipientAddr, genHelper.Props.boxValue),
      (Address @@ genHelper.Props.recipientAddr, genHelper.Props.boxValue))
    val sig = PrivateKey25519Companion.sign(
      key,
      PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
    )
    PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
  }

  val paymentTransactionInvalid: PaymentTransaction = {
    val proposition = key.publicImage
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val outputs = IndexedSeq((Address @@ genHelper.Props.recipientAddr, genHelper.Props.boxValue))
    val sig = Signature25519(Signature @@ Random.randomBytes(64))
    PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
  }

  val coinbaseTransaction = CoinbaseTransaction(
    PublicKey25519Proposition(PublicKey @@ Random.randomBytes()),
    timestamp,
    Signature25519(Signature @@ Random.randomBytes(64)),
    IndexedSeq(ADKey @@ Random.randomBytes(), ADKey @@ Random.randomBytes()),
    999L,
    Height @@ 0
  )

  val addPubKeyInfoTransaction: AddPubKeyInfoTransaction = {
    val proposition = key.publicImage
    val fee = genHelper.Props.txFee
    val change = 20L
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address),
      genHelper.genAssetBox(Address @@ key.publicImage.address), genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val pubKeyBytes = PublicKey @@ Random.randomBytes()
    val pubKeyProofBytes = Signature @@ Random.randomBytes(64)
    val pubKeyInfoBytes = Random.randomBytes(40)
    val pubKeyTypeId = 99.toByte
    val sig = PrivateKey25519Companion.sign(
      key,
      AddPubKeyInfoTransaction.getMessageToSign(
        proposition, fee, timestamp, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
    )
    AddPubKeyInfoTransaction(
      proposition, fee, timestamp, sig, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
  }

  val pubKeyInfoBox: PubKeyInfoBox =
    PubKeyInfoBox(
      AddressProposition(Address @@ key.publicImage.address),
      999L,
      PublicKey @@ Random.randomBytes(),
      Signature @@ Random.randomBytes(64),
      Random.randomBytes(40),
      99.toByte
    )
}
