package encry.modifiers

import encry.crypto.PublicKey25519
import encry.local.TestHelper
import encry.modifiers.mempool._
import encry.modifiers.state.box.PubKeyInfoBox
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.state.box.proposition.AccountProposition
import encry.view.history.Height
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.utils.Random

object InstanceFactory {

  private val genHelper = TestHelper
  private val secret = genHelper.getOrGenerateKeys(genHelper.Props.keysFilePath).head
  private val publicKey = secret.publicImage
  private val timestamp = System.currentTimeMillis()

  def paymentTransactionValid(pubKey: PublicKey25519 = publicKey): EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(pubKey.address),
      genHelper.genAssetBox(pubKey.address))

    TransactionFactory.defaultPaymentTransaction(pubKey, secret, fee, timestamp, useBoxes,
      genHelper.Props.recipientAddr, genHelper.Props.txAmount)
  }

  def paymentTransactionInvalid(pubKey: PublicKey25519 = publicKey): EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(pubKey.address))

    TransactionFactory.defaultPaymentTransaction(pubKey, secret, fee, timestamp, useBoxes,
      genHelper.Props.recipientAddr, genHelper.Props.txAmount)
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
    val useBoxes = IndexedSeq(genHelper.genAssetBox(pubKey.address),
      genHelper.genAssetBox(pubKey.address), genHelper.genAssetBox(pubKey.address)).map(_.id)
    val pubKeyBytes = PublicKey @@ Random.randomBytes()
    val pubKeyProofBytes = Signature @@ Random.randomBytes(64)
    val pubKeyInfoBytes = Random.randomBytes(40)
    val pubKeyTypeId = 99.toByte
    val sig = Signature25519(Curve25519.sign(
      secret.privKeyBytes,
      AddPubKeyInfoTransaction.getMessageToSign(
        pubKey, fee, timestamp, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
    ))
    AddPubKeyInfoTransaction(
      pubKey, fee, timestamp, sig, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
  }

  val pubKeyInfoBox: PubKeyInfoBox =
    PubKeyInfoBox(
      AccountProposition(secret.publicImage.address),
      999L,
      PublicKey @@ Random.randomBytes(),
      Signature @@ Random.randomBytes(64),
      Random.randomBytes(40),
      99.toByte
    )
}
