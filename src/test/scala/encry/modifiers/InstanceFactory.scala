package encry.modifiers

import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool._
import encry.modifiers.state.Keys
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.network.DeliveryManagerTests.DMUtils.{generateDummyHistory, generateNextBlock}
import encry.settings.{Constants, EncryAppSettings, NodeSettings}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.{EncryGenerator, FileHelper, NetworkTimeProvider, TestHelper}
import encry.view.history.EncryHistory
import encry.view.history.History.Height
import encry.view.history.processors.payload.BlockPayloadProcessor
import encry.view.history.processors.proofs.FullStateProofProcessor
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.Input
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, LeafData}
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.iq80.leveldb.Options
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

    Block(header, Payload(header.id, Seq(coinbaseTransaction)), None)
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

  def generateFakeChain(blocksQty: Int): Seq[Block] = {
    val srand = new Scarand()
    (0 until blocksQty).foldLeft(Seq.empty[Block], Seq.empty[AssetBox]) {
      case ((fakeBlockchain, utxo), blockHeight) =>
        val block = if (fakeBlockchain.isEmpty) generateGenesisBlock else {
          val addr = randomAddress
          val txs =
            utxo.map(box => genValidPaymentTxToAddrWithSpentBoxes(IndexedSeq(box), addr)) ++
              Seq(coinbaseTransactionWithDiffSupply(Math.abs(srand.nextInt(1000))))
          val txsRoot = Algos.merkleTreeRoot(txs.map(tx => LeafData @@ tx.id.untag(ModifierId)))
          val header = genHeaderAtHeight(blockHeight, txsRoot)
          val payload = Payload(header.id, txs)
          Block(header, payload, None)
        }
        val newUtxo = block.payload.transactions.flatMap(_.newBoxes)
        (fakeBlockchain :+ block) -> newUtxo.collect { case ab: AssetBox => ab }
    }
  }._1

  def genForkOn(qty: Int,
                addDifficulty: BigInt = 0,
                from: Int,
                to: Int,
                settings: EncryAppSettings): (List[Block], List[Block]) = {
    val history: EncryHistory = generateDummyHistory(settings)
    val forkInterval = (from until to).toList
    (0 until qty).foldLeft((List(history), List.empty[Block] -> List.empty[Block])) {
      case ((histories, blocks), blockHeight) =>
        if (forkInterval.contains(blockHeight)) {
          if (histories.length == 1) {
            val secondHistory = generateDummyHistory(settings)
            blocks._1.foldLeft(secondHistory) {
              case (prevHistory, blockToApply) =>
                prevHistory.append(blockToApply.header).get._1.append(blockToApply.payload).get._1.reportModifierIsValid(blockToApply)
            }
            val nextBlockInFirstChain = generateNextBlock(histories.head)
            val nextBlockInSecondChain = generateNextBlock(secondHistory, additionalDifficulty = addDifficulty)
            (
              List(
                histories.head.append(nextBlockInFirstChain.header).get._1.append(nextBlockInFirstChain.payload).get._1.reportModifierIsValid(nextBlockInFirstChain),
                secondHistory.append(nextBlockInSecondChain.header).get._1.append(nextBlockInSecondChain.payload).get._1.reportModifierIsValid(nextBlockInSecondChain)
              ),
              (blocks._1 :+ nextBlockInFirstChain) -> List(nextBlockInSecondChain)
            )
          } else {
            val nextBlockInFirstChain = generateNextBlock(histories.head)
            val nextBlockInSecondChain = generateNextBlock(histories.last, additionalDifficulty = addDifficulty)
            (
              List(
                histories.head.append(nextBlockInFirstChain.header).get._1.append(nextBlockInFirstChain.payload).get._1.reportModifierIsValid(nextBlockInFirstChain),
                histories.last.append(nextBlockInSecondChain.header).get._1.append(nextBlockInSecondChain.payload).get._1.reportModifierIsValid(nextBlockInSecondChain)
              ),
              (blocks._1 :+ nextBlockInFirstChain) -> (blocks._2 :+ nextBlockInSecondChain)
            )
          }
        } else {
          val block: Block = generateNextBlock(histories.head)
          (
            List(
              histories.head.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
            ),
            (blocks._1 :+ block) -> blocks._2
          )
        }
    }._2
  }

  def generateNextBlock(history: EncryHistory,
                        difficultyDiff: BigInt = 1,
                        prevId: Option[ModifierId] = None,
                        txsQty: Int = 100,
                        additionalDifficulty: BigInt = 0): Block = {
    val previousHeaderId: ModifierId =
      prevId.getOrElse(history.bestHeaderOpt.map(_.id).getOrElse(Header.GenesisParentId))
    val requiredDifficulty: Difficulty = history.bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
      .getOrElse(Constants.Chain.InitialDifficulty)
    val txs = (if (txsQty != 0) genValidPaymentTxs(Scarand.nextInt(txsQty)) else Seq.empty) ++
      Seq(coinbaseTransaction)
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.bestHeaderHeight + 1,
      difficulty = Difficulty @@ (requiredDifficulty + difficultyDiff + additionalDifficulty),
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )
    Block(header, Payload(header.id, txs), None)
  }

  def generateNextBlockValidForHistory(history: EncryHistory,
                                       difficultyDiff: BigInt = 0,
                                       score: BigInt = 1,
                                       prevBlock: Option[Block],
                                       txs: Seq[Transaction]): Block = {
    val previousHeaderId: ModifierId = prevBlock.map(_.id).getOrElse(Header.GenesisParentId)
    val requiredDifficulty: Difficulty = prevBlock.map(b => history.requiredDifficultyAfter(b.header))
      .getOrElse(Constants.Chain.InitialDifficulty)
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.bestHeaderHeight + 1,
      difficulty = Difficulty @@ (requiredDifficulty + difficultyDiff),
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )
    Block(header, Payload(header.id, txs), None)
  }

  def generateDummyHistory(settingsEncry: EncryAppSettings): EncryHistory = {

    val indexStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = 0)
    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vldbInit = VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settingsEncry.levelDB))
    val storage: HistoryStorage = new HistoryStorage(vldbInit)

    val ntp: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)

    new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
      override protected val settings: EncryAppSettings = settingsEncry
      override protected val nodeSettings: NodeSettings = settings.node
      override protected val historyStorage: HistoryStorage = storage
      override protected val timeProvider: NetworkTimeProvider = ntp
    }
  }
}