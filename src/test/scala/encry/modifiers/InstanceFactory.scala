package encry.modifiers

import encry.consensus.EncrySupplyController
import encry.modifiers.mempool._
import encry.modifiers.state.Keys
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper, NetworkTimeProvider, TestHelper}
import encry.view.history.{History, HistoryHeadersDefaultProcessorComponent, HistoryPayloadsNormalSyncProcessorComponent, HistoryPrivateApi}
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.{Input, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryProposition}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, _}
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.iq80.leveldb.Options
import scorex.crypto.hash.Digest32
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

  def generateGenesisBlock(genesisHeight: Height): Block = {
    val txs: Seq[Transaction] = Seq(coinbaseTransaction)
    val txsRoot: Digest32 = Payload.rootHash(txs.map(_.id))
    val header = genHeader.copy(
      parentId = Header.GenesisParentId,
      height = genesisHeight,
      transactionsRoot = txsRoot
    )
    Block(header, Payload(header.id, Seq(coinbaseTransaction)))
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

  def coinbaseTransactionWithDiffSupply(supply: Long = 10L): Transaction = {
    TransactionFactory.coinbaseTransactionScratch(secret.publicImage, timestamp, supply, 0, Height @@ 100)
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

  def generateFakeChain(blocksQty: Int, genesisHeight: Height): Seq[Block] = {
    val srand = new Scarand()
    (0 until blocksQty).foldLeft(Seq.empty[Block], Seq.empty[AssetBox]) {
      case ((fakeBlockchain, utxo), blockHeight) =>
        val block = if (fakeBlockchain.isEmpty) generateGenesisBlock(genesisHeight) else {
          val addr = randomAddress
          val txs =
            utxo.map(box => genValidPaymentTxToAddrWithSpentBoxes(IndexedSeq(box), addr)) ++
              Seq(coinbaseTransactionWithDiffSupply(Math.abs(srand.nextInt(1000))))
          val txsRoot = Algos.merkleTreeRoot(txs.map(tx => LeafData @@ tx.id.untag(ModifierId)))
          val header = genHeaderAtHeight(blockHeight, txsRoot)
          val payload = Payload(header.id, txs)
          Block(header, payload)
        }
        val newUtxo = block.payload.txs.flatMap(_.newBoxes)
        (fakeBlockchain :+ block) -> newUtxo.collect{case ab: AssetBox => ab}
    }
  }._1

  def coinbaseAt(height: Int): Transaction = {
    val supplyTotal: Amount = EncrySupplyController.supplyAt(Height @@ height, settings.constants)
    TransactionFactory.coinbaseTransactionScratch(secret.publicImage, Scarand.nextLong(), supplyTotal, 0, Height @@ height)
  }


  def generateNextBlock(history: History,
                        difficultyDiff: BigInt = 0,
                        prevId: Option[ModifierId] = None,
                        txsQty: Int = 100,
                        additionalDifficulty: BigInt = 0): Block = {
    val previousHeaderId: ModifierId =
      prevId.getOrElse(history.getBestHeader.map(_.id).getOrElse(Header.GenesisParentId))
    val requiredDifficulty: Difficulty = history.getBestHeader.map(parent =>
      history.requiredDifficultyAfter(parent).getOrElse(Difficulty @@ BigInt(0)))
      .getOrElse(settings.constants.InitialDifficulty)
    val txs = (if (txsQty != 0) genValidPaymentTxs(Scarand.nextInt(txsQty)) else Seq.empty) ++
      Seq(coinbaseAt(history.getBestHeaderHeight + 1))
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.getBestHeaderHeight + 1,
      difficulty = Difficulty @@ (requiredDifficulty + difficultyDiff + additionalDifficulty),
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )

    Block(header, Payload(header.id, txs))
  }

  def genForkOn(qty: Int,
                addDifficulty: BigInt = 0,
                from: Int,
                to: Int,
                settings: EncryAppSettings): (List[Block], List[Block]) = {
    val history: History = generateDummyHistory(settings)
    val forkInterval = (from until to).toList
    (0 until qty).foldLeft((List(history), List.empty[Block] -> List.empty[Block])) {
      case ((histories, blocks), blockHeight) =>
        if (forkInterval.contains(blockHeight)) {
          if (histories.length == 1) {
            val secondHistory = generateDummyHistory(settings)
            blocks._1.foldLeft(secondHistory) {
              case (prevHistory, blockToApply) =>
                prevHistory.append(blockToApply.header)
                prevHistory.append(blockToApply.payload)
                prevHistory.reportModifierIsValid(blockToApply)
                prevHistory
            }
            val nextBlockInFirstChain = generateNextBlock(histories.head)
            val nextBlockInSecondChain = generateNextBlock(secondHistory, additionalDifficulty = addDifficulty)
            histories.head.append(nextBlockInFirstChain.header)
            histories.head.append(nextBlockInFirstChain.payload)
            val a = histories.head.reportModifierIsValid(nextBlockInFirstChain)
            secondHistory.append(nextBlockInSecondChain.header)
            secondHistory.append(nextBlockInSecondChain.payload)
            val b = secondHistory.reportModifierIsValid(nextBlockInSecondChain)
            (List(a, b), (blocks._1 :+ nextBlockInFirstChain) -> List(nextBlockInSecondChain))
          } else {
            val nextBlockInFirstChain = generateNextBlock(histories.head)
            val nextBlockInSecondChain = generateNextBlock(histories.last, additionalDifficulty = addDifficulty)
            histories.head.append(nextBlockInFirstChain.header)
            histories.head.append(nextBlockInFirstChain.payload)
            val a = histories.head.reportModifierIsValid(nextBlockInFirstChain)
            histories.last.append(nextBlockInSecondChain.header)
            histories.last.append(nextBlockInSecondChain.payload)
            val b = histories.last.reportModifierIsValid(nextBlockInSecondChain)
            (List(a, b), (blocks._1 :+ nextBlockInFirstChain) -> (blocks._2 :+ nextBlockInSecondChain))
          }
        } else {
          val block: Block = generateNextBlock(histories.head)
          histories.head.append(block.header)
          histories.head.append(block.payload)
          val a = histories.head.reportModifierIsValid(block)
          (List(a), (blocks._1 :+ block) -> blocks._2)
        }
    }._2
  }

  def generateDummyHistory(settingsEncry: EncryAppSettings): History = {

    val indexStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vldbInit = VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settingsEncry.levelDB))
    val storage: HistoryStorage = new HistoryStorage(vldbInit)

    val ntp: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)

    new History with HistoryPrivateApi with HistoryPayloadsNormalSyncProcessorComponent
      with HistoryHeadersDefaultProcessorComponent {
      override val settings: EncryAppSettings = settingsEncry
      override  val historyStorage: HistoryStorage = storage
    }
  }
}