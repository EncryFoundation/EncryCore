package encry.modifiers

import encry.account.Account
import encry.modifiers.mempool._
import encry.modifiers.state.Keys
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.state.box.{AssetBox, AssetCreationBox}
import encry.utils.TestHelper
import encry.view.history.Height

import scala.util.{Random => Scarand}

trait InstanceFactory extends Keys {

  private val genHelper = TestHelper
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
        EncryProposition.accountLock(Account(secret.publicImage.address)),
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
    TransactionFactory.coinbaseTransactionScratch(secret.publicImage, timestamp, useBoxes, 0)
  }

  lazy val AssetBoxI: AssetBox =
    AssetBox(
      EncryProposition.accountLock(Account(secret.publicImage.address)),
      999L,
      100000L
    )

  lazy val AssetCreationBoxI: AssetCreationBox =
    AssetCreationBox(
      EncryProposition.accountLock(Account(secret.publicImage.address)),
      999L,
      10000L,
      "SYM"
    )

  lazy val OpenAssetBoxI: AssetBox =
    AssetBox(
      EncryProposition.open,
      999L,
      100000L
    )
}