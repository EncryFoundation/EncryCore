package encry.view.wallet.storage

import java.io.File

import encry.account.Address
import encry.local.TestHelper
import encry.modifiers.mempool.PaymentTransaction
import encry.settings.{Algos, EncryAppSettings}
import encry.view.wallet.EncryWallet
import encry.view.wallet.keys.KeyManager
import io.iohk.iodb.LSMStore
import org.scalatest.FunSuite
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.utils.Random

class WalletStorageTest extends FunSuite {

  test("WalletStorage"){

    lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

    val walletDir: File = new File(s"${System.getProperty("user.dir")}/test-data/wallet")
    walletDir.mkdir()

    val walletStorage = new LSMStore(walletDir, keySize = 32, keepVersions = 0)

    val keysDir: File = new File(s"${System.getProperty("user.dir")}/test-data/keys")

    keysDir.mkdir()

    val keyStorage = new LSMStore(keysDir, keySize = 32, keepVersions = 0)

    def readOrGenerate(keysStore: LSMStore,
                       settings: EncryAppSettings,
                       password: Option[Array[Byte]] = Option(Array[Byte]()),
                       seed: Array[Byte] = Random.randomBytes()): KeyManager = {

      val keyManager = KeyManager(keysStore, settings.keyManagerSettings, password)

      if (keyManager.keys.isEmpty) {
        keyManager.initStorage(seed)
        if (settings.keyManagerSettings.lock && !keyManager.isLocked) {
          keyManager.lock()
        }
      }

      keyManager
    }

    val keyManager = readOrGenerate(keyStorage, encrySettings, Option(Algos.hash("Password")))

    val wallet: EncryWallet = new EncryWallet(walletStorage, keyManager)

    //wallet.keyManager.initStorage("testSeed".getBytes())

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val validTxs = keys.map { key =>
      val proposition = key.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ wallet.keyManager.keys.head.publicImage.address, factory.Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        key,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val spentTx = validTxs.map { tx =>
      val proposition = wallet.keyManager.keys.head.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(tx.newBoxes.last.id)
      val outputs = IndexedSeq((Address @@ keys.head.publicImage.address, factory.Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        wallet.keyManager.keys.head,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val tx = validTxs :+ spentTx

    val trxCount = 50

    val spentTxCount = 25

    validTxs.slice(0, trxCount).foreach(wallet.walletStorage.putTransaction)

    spentTx.slice(0, spentTxCount).foreach(wallet.walletStorage.putTransaction)

    val expectedBalance = trxCount*factory.Props.boxValue - spentTxCount*factory.Props.boxValue

    assert(expectedBalance == wallet.balance, "Balance not equals")

  }
}
