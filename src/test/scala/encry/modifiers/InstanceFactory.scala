package encry.modifiers

import encry.local.TestHelper
import encry.modifiers.mempool._
import encry.modifiers.state.box.{AssetBox, PubKeyInfoBox}
import encry.modifiers.state.box.proposition.{AccountProposition, OpenProposition}
import encry.view.history.Height
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random

object InstanceFactory {

  private val genHelper = TestHelper
  private val secret = genHelper.getOrGenerateKeys(genHelper.Props.keysFilePath).head
  private val publicKey = secret.publicImage
  private val timestamp = System.currentTimeMillis()

  def paymentTransactionValid: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address),
      genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address, genHelper.Props.txAmount)
  }

  def paymentTransactionInvalid: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, -100, timestamp, useBoxes,
      genHelper.Props.recipientAddr, genHelper.Props.txAmount)
  }

  val coinbaseTransaction: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(secret.publicImage.address))
    TransactionFactory.coinbaseTransactionScratch(secret, timestamp, useBoxes, Height @@ 0)
  }

  def addPubKeyInfoTransaction: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address),
      genHelper.genAssetBox(publicKey.address), genHelper.genAssetBox(publicKey.address))
    val pubKeyBytes = PublicKey @@ Random.randomBytes()
    val pubKeyProofBytes = Signature @@ Random.randomBytes(64)
    val pubKeyInfoBytes = Random.randomBytes(40)
    val pubKeyTypeId = 99.toByte

    TransactionFactory.addPubKeyInfoTransactionScratch(
      secret, fee, timestamp, useBoxes, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
  }

  val assetBox: AssetBox =
    AssetBox(
      AccountProposition(secret.publicImage.address),
      999L,
      100000L
    )

  val openAssetBox: AssetBox =
    AssetBox(
      OpenProposition,
      999L,
      100000L
    )

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
