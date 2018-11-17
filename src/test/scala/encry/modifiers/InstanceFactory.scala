package encry.modifiers

import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool._
import encry.modifiers.state.Keys
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.{Constants, EncryAppSettings, NodeSettings}
import encry.utils.{EncryGenerator, FileHelper, NetworkTimeProvider, TestHelper}
import encry.view.history.EncryHistory
import encry.view.history.History.Height
import encry.view.history.processors.payload.BlockPayloadProcessor
import encry.view.history.processors.proofs.FullStateProofProcessor
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.transaction.Input
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.{Ast, Types}
import scorex.utils.Random

import scala.util.{Random => Scarand}

trait InstanceFactory extends Keys with EncryGenerator {

  private val genHelper = TestHelper

  lazy val fakeTransaction: Transaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address.address),
      genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address.address, 12345678L)
  }

  lazy val paymentTransactionValid: Transaction = {
    val fee: Amount = genHelper.Props.txFee
    val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genHelper.genAssetBox(publicKey.address.address),
      genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address.address, genHelper.Props.txAmount)
  }

  def generateGenesisBlock: Block = {

    val header = genHeader.copy(parentId = Header.GenesisParentId, height = Constants.Chain.GenesisHeight)

    Block(header, Payload(header.id, Seq.empty), None)
  }

  def paymentTransactionDynamic: Transaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = (0 to 5).map(_ => {
      AssetBox(
        EncryProposition.pubKeyLocked(secret.publicImage.pubKeyBytes),
        Scarand.nextLong(),
        999L
      )
    })

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, Scarand.nextLong(), useBoxes,
      publicKey.address.address, genHelper.Props.txAmount)
  }

  lazy val paymentTransactionInvalid: Transaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, -100, timestamp, useBoxes,
      randomAddress, genHelper.Props.txAmount)
  }

  lazy val coinbaseTransaction: Transaction = {
    TransactionFactory.coinbaseTransactionScratch(secret.publicImage, timestamp, 10L, 0, Height @@ 100)
  }

  lazy val AssetBoxI: AssetBox =
    AssetBox(
      EncryProposition.pubKeyLocked(secret.publicImage.pubKeyBytes),
      999L,
      100000L
    )

  lazy val OpenAssetBoxI: AssetBox =
    AssetBox(
      EncryProposition.open,
      999L,
      100000L
    )

  lazy val Contract: CompiledContract = CompiledContract(
    List("state" -> Types.EncryState),
    Expr.If(
      Expr.Compare(
        Expr.Attribute(
          Expr.Name(
            Ast.Ident("state"),
            Types.EncryState
          ),
          Ast.Ident("height"),
          Types.PInt
        ),
        List(Ast.CompOp.GtE),
        List(Expr.IntConst(1000L))
      ),
      Expr.True,
      Expr.False,
      Types.PBoolean
    )
  )

  lazy val UnsignedInput: Input = Input(ADKey @@ Random.randomBytes(), Left(Contract), List.empty)

  def generateNextBlock(history: EncryHistory): Block = {
    val previousHeaderId = history.bestBlockIdOpt.getOrElse(Header.GenesisParentId)
    val difficulty = history.bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
      .getOrElse(Constants.Chain.InitialDifficulty)
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.bestHeaderHeight + 1,
      difficulty = difficulty
    )
    Block(header, Payload(header.id, Seq(coinbaseTransaction)), None)
  }

  def generateDummyHistory: EncryHistory = {
    val settings: EncryAppSettings = EncryAppSettings.read

    val indexStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val storage: HistoryStorage = new HistoryStorage(indexStore, objectsStore)

    val ntp: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

    new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
      override protected val nodeSettings: NodeSettings = settings.node
      override protected val historyStorage: HistoryStorage = storage
      override protected val timeProvider: NetworkTimeProvider = ntp
    }
  }
}