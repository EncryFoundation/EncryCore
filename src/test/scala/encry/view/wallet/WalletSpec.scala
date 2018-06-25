package encry.view.wallet

import encry.ModifierId
import encry.modifiers.InstanceFactory
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.MonetaryBox
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.TestHelper.Props
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.wallet.keys.KeyManager
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.Random

class WalletSpec extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  lazy val settings: EncryAppSettings = EncryAppSettings.read

  property("Balance count (intrinsic coins only)"){

    val blockHeader: EncryBlockHeader = genHeader

    val walletStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = Constants.DefaultKeepVersions)

    val keyManager = KeyManager(new LSMStore(FileHelper.getRandomTempDir, keepVersions = Constants.DefaultKeepVersions), settings.keyManager, None)

    keyManager.initStorage(Random.randomBytes())

    val wallet = EncryWallet(walletStore, keyManager)

    val validTxs: Seq[EncryTransaction] = genValidPaymentTxsToAddr(4, keyManager.keys.head.publicImage.address)

    val correctBalance: Long = validTxs.foldLeft(0L) {
      case (sum, transaction) => sum + transaction.newBoxes.foldLeft(0L) {
        case (boxSum, bx) =>
          bx match {
          case ac: MonetaryBox
            if keyManager.keys.exists(privKey => EncryProposition.accountLock(privKey.publicImage.address) == ac.proposition) =>
              boxSum + ac.amount
          case _ =>
            boxSum
          }
      }
    }

    val blockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val block = EncryBlock(blockHeader, blockPayload, None)

    wallet.scanPersistent(block)

    wallet.walletStorage.getTokenBalanceById(Constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance
  }

  property("Balance count (intrinsic coins + tokens)"){

    val txsQty: Int = 4

    val blockHeader: EncryBlockHeader = genHeader

    val walletStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = Constants.DefaultKeepVersions)

    val keyManager = KeyManager(new LSMStore(FileHelper.getRandomTempDir, keepVersions = Constants.DefaultKeepVersions), settings.keyManager, None)

    keyManager.initStorage(Random.randomBytes())

    val wallet = EncryWallet(walletStore, keyManager)

    val validTxs: Seq[EncryTransaction] = genValidPaymentTxsToAddrWithDiffTokens(txsQty, keyManager.keys.head.publicImage.address)

    val blockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val block = EncryBlock(blockHeader, blockPayload, None)

    wallet.scanPersistent(block)

    wallet.getBalances.foldLeft(0L){_ + _._2} shouldEqual txsQty * Props.boxValue
  }
}
