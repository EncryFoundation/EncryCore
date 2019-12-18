package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.{EncryAppSettings, LevelDBSettings, Settings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.utils.TestHelper.Props
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.avlTree.{AvlTree, LeafNode}
import encry.view.state.avlTree.utils.implicits.Hashable
import encry.view.state.{UtxoState, UtxoStateReader}
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.{AssetBox, DataBox, DataBoxSerializer, EncryProposition, MonetaryBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId}
import org.scalatest.{Matchers, PropSpec}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar

class WalletSpec extends PropSpec with Matchers with InstanceFactory with EncryGenerator with StrictLogging with Settings with MockitoSugar {

  val dummyLevelDBSettings = LevelDBSettings(5)

  property("Balance count (intrinsic coins only).") {

    val dir = FileHelper.getRandomTempDir

    val aM = AccountManager.init(
      "another accuse index island little scissors insect little absurd island keep valid",
      "encry",
      settings.copy(directory = dir.getAbsolutePath))

    val wallet: EncryWallet = EncryWallet.readOrGenerate(
      FileHelper.getRandomTempDir,
      FileHelper.getRandomTempDir,
      settings
    )

    val accountManager: AccountManager = wallet.accountManagers.head

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

    wallet.walletStorage.getTokenBalanceById(settings.constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance

    wallet.scanPersistent(secondBlock)

    wallet.walletStorage.getTokenBalanceById(settings.constants.IntrinsicTokenId).getOrElse(0L) shouldEqual correctBalance - useBox.amount

    logger.info(s"tmp dir size: ${dir.length()}")
  }

  property("Balance count (intrinsic coins + tokens)") {

    val dir = FileHelper.getRandomTempDir

    val txsQty: Int = 4

    val blockHeader: Header = genHeader

    val aM = AccountManager.init(
      "another accuse index island little scissors insect little absurd island keep valid",
      "encry",
      settings.copy(directory = dir.getAbsolutePath))

    val wallet: EncryWallet = EncryWallet.readOrGenerate(FileHelper.getRandomTempDir,
      FileHelper.getRandomTempDir,
      settings)

    val keyManager: AccountManager = wallet.accountManagers.head

    val validTxs: Seq[Transaction] = genValidPaymentTxsToAddrWithDiffTokens(txsQty, keyManager.mandatoryAccount.publicImage.address.address)

    val blockPayload: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)

    val block: Block = Block(blockHeader, blockPayload)

    wallet.scanPersistent(block)

    wallet.getBalances.foldLeft(0L)(_ + _._2) shouldEqual txsQty * Props.boxValue
  }

  property("Balance count (intrinsic coins + tokens) for multiple accounts") {

    val dataBox = DataBox(EncryProposition.heightLocked(Height @@ 10), 0L, Array.emptyByteArray)

    import encry.view.state.avlTree.utils.implicits.Instances._

    val rootNode: LeafNode[StorageKey, StorageValue] =
      LeafNode(StorageKey @@ Array(DataBox.`modifierTypeId`), StorageValue @@ DataBoxSerializer.toBytes(dataBox))
    val storageMock = mock[VersionalStorage]
    val tree = AvlTree(rootNode, storageMock)
    val stateMock = mock[UtxoStateReader](RETURNS_DEEP_STUBS)
    when(stateMock.tree).thenReturn(tree)

    val seed = "another accuse index island little scissors insect little absurd island keep valid"
    val alsoSeed = "another accuse index island little island absurd little absurd scissors keep valid"

    val dir = FileHelper.getRandomTempDir

    val aM = AccountManager.init(
      "another accuse index island little scissors insect little insect island keep valid",
      "encry",
      settings.copy(directory = dir.getAbsolutePath))

    val txsQty: Int = 4

    val blockHeader: Header = genHeader

    val wallet: EncryWallet = EncryWallet.readOrGenerate(FileHelper.getRandomTempDir,
      FileHelper.getRandomTempDir,
      settings)
      .addAccount(seed, settings.wallet.map(_.password).get, stateMock).toOption.get

    val keyManagerOne = wallet.accountManagers.head

    val keyManagerTwo = wallet.accountManagers(1)

    val extraAcc = keyManagerTwo.createAccount(Some(alsoSeed))

    val validTxs1: Seq[Transaction] = genValidPaymentTxsToAddr(txsQty, keyManagerOne.mandatoryAccount.publicImage.address.address)
    val validTxs2: Seq[Transaction] = genValidPaymentTxsToAddr(txsQty - 1, keyManagerTwo.mandatoryAccount.publicImage.address.address)
    val validTxs3: Seq[Transaction] = genValidPaymentTxsToAddr(txsQty - 2, extraAcc.publicImage.address.address)
    val validTxstoOther: Seq[Transaction] = genValidPaymentTxsToAddr(txsQty - 3, randomAddress)

    val blockPayload: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs1 ++ validTxs2 ++ validTxs3 ++ validTxstoOther)

    val block: Block = Block(blockHeader, blockPayload)

    wallet.scanPersistent(block)

    val addr1 = Algos.encode(keyManagerOne.mandatoryAccount.publicKeyBytes)
    val addr2 = Algos.encode(keyManagerTwo.mandatoryAccount.publicKeyBytes)
    val addr3 = Algos.encode(extraAcc.publicKeyBytes)

    wallet.getBalances.filter(_._1._1 == addr1).map(_._2).sum shouldEqual txsQty * Props.boxValue
    wallet.getBalances.filter(_._1._1 == addr2).map(_._2).sum shouldEqual (txsQty - 1) * Props.boxValue
    wallet.getBalances.filter(_._1._1 == addr3).map(_._2).sum shouldEqual (txsQty - 2) * Props.boxValue
  }
}