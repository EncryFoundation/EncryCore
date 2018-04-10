package encry.modifiers

import encry.local.TestHelper
import encry.modifiers.mempool._
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.proposition.{AccountProposition, OpenProposition}
import encry.view.history.Height

import scala.util.{Random => Scarand}

trait InstanceFactory {

  private val genHelper = TestHelper
  private val secret = genHelper.getOrGenerateKeys(genHelper.Props.keysFilePath).head
  private val publicKey = secret.publicImage
  private val timestamp = System.currentTimeMillis()

  lazy val fakeTransaction: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address),
      genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address, 12345678L)
  }

  lazy val paymentTransactionValid: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address),
      genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address, genHelper.Props.txAmount)
  }

  def paymentTransactionDynamic: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = (0 to 5).map(_ => {
      AssetBox(
        AccountProposition(secret.publicImage.address),
        Scarand.nextLong(),
        999L
      )
    })

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, Scarand.nextLong(), useBoxes,
      publicKey.address, genHelper.Props.txAmount)
  }

  lazy val paymentTransactionInvalid: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, -100, timestamp, useBoxes,
      genHelper.Props.recipientAddr, genHelper.Props.txAmount)
  }

  lazy val coinbaseTransaction: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(secret.publicImage.address))
    TransactionFactory.coinbaseTransactionScratch(secret, timestamp, useBoxes, Height @@ 0)
  }

  lazy val assetBox: AssetBox =
    AssetBox(
      AccountProposition(secret.publicImage.address),
      999L,
      100000L
    )

  lazy val openAssetBox: AssetBox =
    AssetBox(
      OpenProposition,
      999L,
      100000L
    )
}
