package encry.view.wallet.storage

import encry.account.Address
import encry.local.TestHelper
import encry.modifiers.mempool.PaymentTransaction
import encry.settings.EncryAppSettings
import encry.view.wallet.EncryWallet
import encry.view.wallet.storage.WalletStorageManager
import org.scalatest.FunSuite
import scorex.core.transaction.state.PrivateKey25519Companion

class WalletStorageManagerTest extends FunSuite {

  test("Wallet Data Manager Test"){

    lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

    val wallet: EncryWallet = EncryWallet.readOrGenerate(encrySettings)

    val wdm = WalletStorageManager.readOrGenerate(encrySettings, wallet)

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val validTxs = keys.map { key =>
      val proposition = key.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ wallet.keyStorage.keys.head.publicImage.address, factory.Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        key,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    wdm.initStorage()

    val trxCount = 50

    validTxs.slice(0, trxCount).foreach(wdm.putTx)

    assert(trxCount*factory.Props.boxValue == wdm.getBalance, "Balance not equals")

  }

}
