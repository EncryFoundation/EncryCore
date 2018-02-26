package encry.modifiers

import encry.account.Address
import encry.crypto.{PublicKey25519, Signature25519}
import encry.local.TestHelper
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.mempool.{AddPubKeyInfoTransaction, CoinbaseTransaction, PaymentTransaction}
import encry.modifiers.state.box.PubKeyInfoBox
import encry.modifiers.state.box.proposition.AccountProposition
import encry.view.history.Height
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.utils.Random

object InstanceFactory {

  private val genHelper = TestHelper
  private val key = genHelper.getOrGenerateKeys(genHelper.Props.keysFilePath).head
  private val publicKey = PublicKey25519(key.publicKeyBytes)
  private val timestamp = System.currentTimeMillis()

  def paymentTransactionValid(pubKey: PublicKey25519 = publicKey): PaymentTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address),
      genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val outputs = IndexedSeq(
      TransferDirective(genHelper.Props.recipientAddr, genHelper.Props.boxValue, 1),
      TransferDirective(genHelper.Props.recipientAddr, genHelper.Props.boxValue, 2),
      TransferDirective(genHelper.Props.recipientAddr, genHelper.Props.boxValue, 3)
    )
    val sig = Signature25519(Curve25519.sign(
      key.privKeyBytes,
      PaymentTransaction.getMessageToSign(pubKey, fee, timestamp, useBoxes, outputs)
    ))
    PaymentTransaction(pubKey, fee, timestamp, sig, useBoxes, outputs)
  }

  def paymentTransactionInvalid(pubKey: PublicKey25519 = publicKey): PaymentTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val outputs = IndexedSeq(
      TransferDirective(genHelper.Props.recipientAddr, genHelper.Props.boxValue, 1),
      TransferDirective(genHelper.Props.recipientAddr, genHelper.Props.boxValue, 2)
    )
    val sig = Signature25519(Signature @@ Random.randomBytes(64))
    PaymentTransaction(pubKey, fee, timestamp, sig, useBoxes, outputs)
  }

  val coinbaseTransaction = CoinbaseTransaction(
    PublicKey25519(PublicKey @@ Random.randomBytes()),
    timestamp,
    Signature25519(Signature @@ Random.randomBytes(64)),
    IndexedSeq(ADKey @@ Random.randomBytes(), ADKey @@ Random.randomBytes()),
    999L,
    Height @@ 0
  )

  def addPubKeyInfoTransaction(pubKey: PublicKey25519 = publicKey): AddPubKeyInfoTransaction = {
    val fee = genHelper.Props.txFee
    val change = 20L
    val useBoxes = IndexedSeq(genHelper.genAssetBox(Address @@ key.publicImage.address),
      genHelper.genAssetBox(Address @@ key.publicImage.address), genHelper.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
    val pubKeyBytes = PublicKey @@ Random.randomBytes()
    val pubKeyProofBytes = Signature @@ Random.randomBytes(64)
    val pubKeyInfoBytes = Random.randomBytes(40)
    val pubKeyTypeId = 99.toByte
    val sig = Signature25519(Curve25519.sign(
      key.privKeyBytes,
      AddPubKeyInfoTransaction.getMessageToSign(
        pubKey, fee, timestamp, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
    ))
    AddPubKeyInfoTransaction(
      pubKey, fee, timestamp, sig, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
  }

  val pubKeyInfoBox: PubKeyInfoBox =
    PubKeyInfoBox(
      AccountProposition(Address @@ key.publicImage.address),
      999L,
      PublicKey @@ Random.randomBytes(),
      Signature @@ Random.randomBytes(64),
      Random.randomBytes(40),
      99.toByte
    )
}
