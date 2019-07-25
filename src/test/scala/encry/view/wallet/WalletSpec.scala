package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.{EncryAppSettings, LevelDBSettings}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, WalletVersionalLevelDBCompanion}
import encry.utils.TestHelper.Props
import encry.utils.{EncryGenerator, FileHelper}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.{AssetBox, MonetaryBox}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}

class WalletSpec extends PropSpec with Matchers with InstanceFactory with EncryGenerator with StrictLogging {

  lazy val settings: EncryAppSettings = EncryAppSettings.read

  val dummyLevelDBSettings = LevelDBSettings(5)

  property("Balance count (intrinsic coins only).") {

    val dir = FileHelper.getRandomTempDir

    val db: DB = {
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    val accountManagerStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0, keySize = 33)

    val accountManager: AccountManager = AccountManager(accountManagerStore)

    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))

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

    val firstBlock: Block = Block(genHeader, blockPayload)

    val blockPayloadWithSpentTx: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), Seq(spentTx))

    val secondBlock: Block = Block(genHeader, blockPayloadWithSpentTx)

    wallet.scanPersistent(firstBlock)

    wallet.walletStorage.getTokenBalanceById(TestNetConstants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance

    wallet.scanPersistent(secondBlock)

    wallet.walletStorage.getTokenBalanceById(TestNetConstants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance - useBox.amount

    logger.info(s"tmp dir size: ${dir.length()}")
  }

  property("Balance count (intrinsic coins + tokens)") {

    val dir = FileHelper.getRandomTempDir

    val db: DB = {
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    val txsQty: Int = 4

    val blockHeader: Header = genHeader

    val accountManagerStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0, keySize = 33)

    val keyManager: AccountManager = AccountManager(accountManagerStore)

    val walletStorage = WalletVersionalLevelDBCompanion(db, dummyLevelDBSettings)

    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))

    val validTxs: Seq[Transaction] = genValidPaymentTxsToAddrWithDiffTokens(txsQty, keyManager.mandatoryAccount.publicImage.address.address)

    val blockPayload: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val block: Block = Block(blockHeader, blockPayload)

    wallet.scanPersistent(block)

    wallet.getBalances.foldLeft(0L)(_ + _._2) shouldEqual txsQty * Props.boxValue
  }
}