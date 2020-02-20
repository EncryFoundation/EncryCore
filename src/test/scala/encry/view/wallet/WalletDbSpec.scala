package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.{ EncryAppSettings, LevelDBSettings, Settings }
import encry.storage.levelDb.versionalLevelDB.{
  LevelDbDiff,
  LevelDbFactory,
  VersionalLevelDB,
  VersionalLevelDBCompanion
}
import encry.utils.{ EncryGenerator, FileHelper }
import org.encryfoundation.common.modifiers.history.{ Block, Payload }
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{ AssetBox, EncryBaseBox, MonetaryBox }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ADKey, ModifierId }
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import org.iq80.leveldb.{ DB, Options }
import org.scalatest.{ Matchers, PropSpec, WordSpecLike }
import org.scalatest.mockito.MockitoSugar
import scorex.utils.Random

class WalletDbSpec
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with EncryGenerator
    with StrictLogging
    with Settings
    with MockitoSugar {

  val levelDbElemsQty = 10

  val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(5)

  val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)

  val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

  val settingsR: EncryAppSettings = EncryAppSettings.read()

  def state = {
    def init: WalletDBImpl               = new WalletDBImpl(vldbInit, settingsR)
    val api: WalletDBImpl                = init
    val validTxs: Seq[Transaction]       = genValidPaymentTxs(3)
    val useBox: AssetBox                 = validTxs.head.newBoxes.head.asInstanceOf[AssetBox]
    val spentTx: Transaction             = genValidPaymentTxToAddrWithSpentBoxes(IndexedSeq(useBox), randomAddress)
    val blockPayload: Payload            = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)
    val blockPayloadWithSpentTx: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), Seq(spentTx))
    val newTxs: List[EncryBaseBox]       = blockPayload.txs.flatMap(_.newBoxes).toList
    val spentTxs: List[EncryBaseBox]     = blockPayloadWithSpentTx.txs.flatMap(_.newBoxes).toList
    val res: Unit =
      api.updateWallet(ModifierId @@ Random.randomBytes(), newTxs, spentTxs, settingsR.constants.IntrinsicTokenId)
    (api, newTxs, spentTxs)
  }

  def initTestState: (WalletDB, Seq[EncryBaseBox]) = {
    val levelDB: DB                       = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vlDB: VersionalLevelDB            = VersionalLevelDBCompanion(levelDB, dummyLevelDBSettings)
    def dbInstance: WalletDB              = WalletDB.apply(vlDB, settingsR)
    val boxesToInsert: List[EncryBaseBox] = genValidPaymentTxs(3).flatMap(_.newBoxes).toList
    dbInstance.updateWallet(
      ModifierId @@ Random.randomBytes(),
      boxesToInsert,
      List.empty,
      settings.constants.IntrinsicTokenId
    )
    (dbInstance, boxesToInsert)
  }

  "WalletDb.getBoxById" should {
    "return non empty value for existed box in db" in {
      val (walletDb: WalletDBImpl, inserted: Seq[EncryBaseBox]) = initTestState
      val comparisonResult: Boolean = inserted.forall { box =>
        val boxFromDB: Option[EncryBaseBox] = walletDb.getBoxById(box.id)
        boxFromDB.isDefined && boxFromDB.forall { takenBox =>
          box.bytes.sameElements(takenBox.bytes)
        }
      }
      comparisonResult shouldBe true

      val boxesToInsert: List[EncryBaseBox] = genValidPaymentTxs(5).flatMap(_.newBoxes).toList
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        boxesToInsert,
        inserted.toList,
        settings.constants.IntrinsicTokenId
      )
      val comparisonResultNegative: Boolean = inserted.forall { box =>
        walletDb.getBoxById(box.id).isEmpty
      }
      val comparisonResultPositive: Boolean = boxesToInsert.forall { box =>
        val boxFromDB: Option[EncryBaseBox] = walletDb.getBoxById(box.id)
        boxFromDB.isDefined && boxFromDB.forall { takenBox =>
          box.bytes.sameElements(takenBox.bytes)
        }
      }
      (comparisonResultNegative && comparisonResultPositive) shouldBe true
    }
    "return empty value for non existed box in db" in {
      val (walletDb: WalletDBImpl, _) = initTestState
      walletDb.getBoxById(ADKey @@ Random.randomBytes()).isEmpty shouldBe true
    }
  }

  "WalletDb.getAllWallets" should {
    "return all inserted wallets" in {
      val (walletDb: WalletDBImpl, inserted: Seq[EncryBaseBox]) = initTestState
      val wallets                                               = walletDb.getAllWallets.map(Algos.encode)
      val neededWallets                                         = inserted.map(l => Algos.encode(l.proposition.contractHash)).toSet
      (wallets.size == neededWallets.size && wallets.forall(neededWallets.contains)) shouldBe true

      val boxesToInsert: List[EncryBaseBox] = genValidPaymentTxs(5).flatMap(_.newBoxes).toList
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        boxesToInsert,
        inserted.toList,
        settings.constants.IntrinsicTokenId
      )

      val walletsNew = walletDb.getAllWallets.map(Algos.encode)
      val neededWalletsNew = inserted.map(l => Algos.encode(l.proposition.contractHash)) ++ boxesToInsert.map(
        l => Algos.encode(l.proposition.contractHash)
      )
      (walletsNew.size == neededWalletsNew.size && walletsNew.forall(neededWalletsNew.contains)) shouldBe true
    }
  }

  "Needs to take what was inserted" should {
    "getBoxById" in {
      val (api, newTxs, _) = state
      (api.getBoxById(newTxs.head.id).get.id sameElements newTxs.head.id) shouldBe true
    }
    "amount in storage should be correct" in {
      val (api, newTxs, spentTxs) = state
      val amountInStorage = api.getAllWallets
        .map(ch => api.getTokenBalanceByContractHash(ch, settingsR.constants.IntrinsicTokenId))
        .foldLeft(0L) {
          case (acc, amount) => acc + amount
        }
      val amountToInsert: Long = {
        val newTx: Long = newTxs.map {
          case a: MonetaryBox => a.amount
        }.foldLeft(0L) {
          case (acc, amount) => acc + amount
        }
        val spent: Long = spentTxs.map {
          case a: MonetaryBox => a.amount
        }.foldLeft(0L) {
          case (acc, amount) => acc + amount
        }
        newTx - spent
      }
      val getBalance: Amount = api.getAllWallets.map(x => api.getBalancesByContractHash(x)).foldLeft(0L) {
        case (acc, amount) => acc + amount.values.sum
      }

      amountInStorage shouldEqual amountToInsert
      amountInStorage shouldEqual getBalance
      amountToInsert shouldEqual getBalance

    }
    "should contain intrinsic tokenId" in {
      val (api, _, _) = state
      Algos.encode(api.getTokenIds(api.getAllWallets.head).head) shouldEqual Algos.encode(
        settingsR.constants.IntrinsicTokenId
      )
    }
    "getAllWallets" in {
      val (api, _, _) = state
      api.getAllWallets.nonEmpty shouldBe true
    }
    "getTypedBoxById" in {
      val (api, newTxs, _) = state
      api.getTypedBoxById[AssetBox](newTxs.head.id).isDefined shouldBe true
    }

  }

}
