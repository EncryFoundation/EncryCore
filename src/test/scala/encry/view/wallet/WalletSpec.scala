package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.{AssetBox, MonetaryBox}
import encry.settings.{Constants, EncryAppSettings}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, WalletVersionalLevelDBCompanion}
import encry.utils.TestHelper.Props
import encry.utils.{EncryGenerator, FileHelper}
import io.iohk.iodb.LSMStore
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}

class WalletSpec extends PropSpec with Matchers with InstanceFactory with EncryGenerator with StrictLogging {

  lazy val settings: EncryAppSettings = EncryAppSettings.read

  property("Balance count (intrinsic coins only).") {

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    val accountManagerStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0, keySize = 33)

    val accountManager: AccountManager = AccountManager(accountManagerStore)

    val walletStorage = WalletVersionalLevelDBCompanion.apply(db)

    val wallet: EncryWallet = EncryWallet(walletStorage, accountManager)

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

    val blockPayload: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val firstBlock: Block = Block(genHeader, blockPayload, None)

    val blockPayloadWithSpentTx: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), Seq(spentTx))

    val secondBlock: Block = Block(genHeader, blockPayloadWithSpentTx, None)

    wallet.scanPersistent(firstBlock)

    wallet.walletStorage.getTokenBalanceById(Constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance

    wallet.scanPersistent(secondBlock)

    wallet.walletStorage.getTokenBalanceById(Constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance - useBox.amount
  }

  property("Balance count (intrinsic coins + tokens)") {

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    val txsQty: Int = 4

    val blockHeader: Header = genHeader

    val accountManagerStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0, keySize = 33)

    val keyManager: AccountManager = AccountManager(accountManagerStore)

    val walletStorage = WalletVersionalLevelDBCompanion(db)

    val wallet: EncryWallet = EncryWallet(walletStorage, keyManager)

    val validTxs: Seq[Transaction] = genValidPaymentTxsToAddrWithDiffTokens(txsQty, keyManager.mandatoryAccount.publicImage.address.address)

    val blockPayload: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val block: Block = Block(blockHeader, blockPayload, None)

    wallet.scanPersistent(block)

    wallet.getBalances.foldLeft(0L)(_ + _._2) shouldEqual txsQty * Props.boxValue
  }
}