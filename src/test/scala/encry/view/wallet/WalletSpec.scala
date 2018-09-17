package encry.view.wallet

import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.InstanceFactory
import encry.modifiers.history.block.Block
import encry.modifiers.history.block.header.Header
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.{AssetBox, MonetaryBox}
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.TestHelper.Props
import encry.utils.{EncryGenerator, FileHelper}
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}

class WalletSpec extends PropSpec with Matchers with InstanceFactory with EncryGenerator {

  lazy val settings: EncryAppSettings = EncryAppSettings.read

  property("Balance count (intrinsic coins only)") {

    val walletStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)

    val accountManagerStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0, keySize = 33)

    val accountManager: AccountManager = AccountManager(accountManagerStore)

    val wallet: EncryWallet = EncryWallet(walletStore, accountManager)

    val validTxs: Seq[Transaction] = genValidPaymentTxsToAddr(4, accountManager.mandatoryAccount.publicImage.address.address)

    val useBox: AssetBox = validTxs.head.newBoxes.head.asInstanceOf[AssetBox]

    val spentTx: Transaction = genValidPaymentTxToAddrWithSpentBoxes(IndexedSeq(useBox), randomAddress)

    val correctBalance: Long = validTxs.foldLeft(0L) {
      case (sum, transaction) => sum + transaction.newBoxes.foldLeft(0L) {
        case (boxSum, bx) =>
          bx match {
            case ac: MonetaryBox if wallet.propositions.exists(_.contractHash sameElements bx.proposition.contractHash) => boxSum + ac.amount
            case _ => boxSum
          }
      }
    }

    val blockPayload: EncryBlockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val firstBlock: Block = Block(genHeader, blockPayload, None)

    val blockPayloadWithSpentTx: EncryBlockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), Seq(spentTx))

    val secondBlock: Block = Block(genHeader, blockPayloadWithSpentTx, None)

    wallet.scanPersistent(firstBlock)

    wallet.walletStorage.getTokenBalanceById(Constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance

    wallet.scanPersistent(secondBlock)

    wallet.walletStorage.getTokenBalanceById(Constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance - useBox.amount
  }

  property("Balance count (intrinsic coins + tokens)") {

    val txsQty: Int = 4

    val blockHeader: Header = genHeader

    val walletStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)

    val accountManagerStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0, keySize = 33)

    val keyManager: AccountManager = AccountManager(accountManagerStore)

    val wallet: EncryWallet = EncryWallet(walletStore, keyManager)

    val validTxs: Seq[Transaction] = genValidPaymentTxsToAddrWithDiffTokens(txsQty, keyManager.mandatoryAccount.publicImage.address.address)

    val blockPayload: EncryBlockPayload = EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val block: Block = Block(blockHeader, blockPayload, None)

    wallet.scanPersistent(block)

    wallet.getBalances.foldLeft(0L)(_ + _._2) shouldEqual txsQty * Props.boxValue
  }
}
