package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.{ EncryAppSettings, LevelDBSettings, Settings }
import encry.storage.levelDb.versionalLevelDB.{ LevelDbDiff, LevelDbFactory, VersionalLevelDBCompanion }
import encry.utils.{ EncryGenerator, FileHelper }
import org.encryfoundation.common.modifiers.history.{ Block, Payload }
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{ AssetBox, EncryBaseBox, MonetaryBox }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import org.iq80.leveldb.Options
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

    def init: WalletDBImpl = new WalletDBImpl(vldbInit, settingsR)

    val api: WalletDBImpl = init
    val validTxs: Seq[Transaction] = genValidPaymentTxs(3)
    val useBox: AssetBox = validTxs.head.newBoxes.head.asInstanceOf[AssetBox]
    val spentTx: Transaction = genValidPaymentTxToAddrWithSpentBoxes(IndexedSeq(useBox), randomAddress)
    val blockPayload: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), validTxs)
    val blockPayloadWithSpentTx: Payload = Payload(ModifierId @@ Array.fill(32)(19: Byte), Seq(spentTx))
    val newTxs: List[EncryBaseBox] = blockPayload.txs.flatMap(_.newBoxes).toList
    val spentTxs: List[EncryBaseBox] = blockPayloadWithSpentTx.txs.flatMap(_.newBoxes).toList
    val res: Unit =
      api.updateWallet(ModifierId @@ Random.randomBytes(), newTxs, spentTxs, settingsR.constants.IntrinsicTokenId)


  "Needs to take what was inserted" should {
    "ids are the same" in {

      (api.getBoxById(newTxs.head.id).get.id sameElements newTxs.head.id) shouldBe true
      api.getAllWallets.nonEmpty shouldBe true
    }
    "amount in storage should be correct" in {
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
      Algos.encode(api.getTokenIds(api.getAllWallets.head).head) shouldEqual Algos.encode(
        settingsR.constants.IntrinsicTokenId
      )
    }
    "" in {
      api.getAssetBoxesByPredicate(api.getAllWallets.head, x => x.map(_.amount).sum > 0).nonEmpty shouldBe true
    }
  }

}
