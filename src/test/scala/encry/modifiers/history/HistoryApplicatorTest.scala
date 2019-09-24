package encry.modifiers.history

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DeliveryManagerTests.DMUtils.generateNextBlock
import encry.network.NodeViewSynchronizer.ReceivableMessages.{RollbackSucceed, SemanticallySuccessfulModifier}
import encry.settings.Settings
import encry.storage.VersionalStorage
import encry.utils.ChainUtils.{coinbaseTransaction, defaultPaymentTransactionScratch, privKey, _}
import encry.utils.{ChainUtils, FileHelper, TestHelper}
import encry.view.actors.HistoryApplicator.StartModifiersApplicationOnStateApplicator
import encry.view.actors.NodeViewHolder.DownloadRequest
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.actors.{HistoryApplicator, StateApplicator}
import encry.view.history.History
import encry.view.state.{BoxHolder, UtxoState}
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId}
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scorex.crypto.hash.Digest32

import scala.collection.Seq
import scala.concurrent.duration._
import scala.util.Random

class HistoryApplicatorTest extends TestKit(ActorSystem())
  with WordSpecLike
  with ImplicitSender
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with Settings {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  def genHistoryBlocks(initialHistory: History, count: Int): (History, Seq[Block]) = {
    (0 until count).foldLeft(initialHistory, Seq.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header)
        prevHistory.append(block.payload)
        (prevHistory.reportModifierIsValid(block), blocks :+ block)
    }
  }

  def blockToModifiers(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload)

  def generateChain(blockQty: Int, genInvalidBlockFrom: Option[Int] = None): (UtxoState, UtxoState, List[Block]) = {
    val numberOfInputsInOneTransaction = 1 //10
    val transactionsNumberInEachBlock = 1 //1000
    val initialboxCount = 1 //100000

    val initialBoxes: IndexedSeq[AssetBox] = (0 until initialboxCount).map(nonce =>
      genHardcodedBox(privKey.publicImage.address.address, nonce)
    )
    val boxesHolder: BoxHolder = BoxHolder(initialBoxes)

    val genesisBlock: Block = ChainUtils.generateGenesisBlock(Height @@ 0)

    val state: UtxoState = utxoFromBoxHolder(boxesHolder, dir, None, settings, VersionalStorage.LevelDB)
      .applyModifier(genesisBlock).right.get

    val stateGenerationResults: (List[Block], Block, UtxoState, IndexedSeq[AssetBox]) =
      (0 until blockQty).foldLeft(List.empty[Block], genesisBlock, state, initialBoxes) {
        case ((blocks, block, stateL, boxes), height) =>
          val genInvalid = genInvalidBlockFrom.exists(height + 1 >= _)
          val nextBlock: Block =
            if (genInvalid)
              generateNextBlockForStateWithInvalidTrans(block, stateL,
                block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq)
            else
              generateNextBlockForStateWithSpendingAllPreviousBoxes(block, stateL,
                block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq)
          val stateN: UtxoState = if (genInvalid) stateL else stateL.applyModifier(nextBlock).right.get
          (blocks :+ nextBlock,
            nextBlock, stateN, boxes.drop(transactionsNumberInEachBlock * numberOfInputsInOneTransaction)
          )
      }
    (state, stateGenerationResults._3, genesisBlock +: stateGenerationResults._1)
  }

  def applyBlocksToHistory(history: History, blocks: Seq[Block]): History =
    blocks.foldLeft(history) {
      case (prevHistory, block) =>
        prevHistory.append(block.header)
        prevHistory.append(block.payload)
        prevHistory.reportModifierIsValid(block)
    }

  def printIds(blocks: List[Block]) = {
    blocks.foreach { b =>
      println(s"header.timestamp: ${b.header.timestamp}")
      println(s"header.id: ${Algos.encode(b.header.id)}")
      println(s"header.payloadId: ${Algos.encode(b.header.payloadId)}")
      println(s"payload.id: ${Algos.encode(b.payload.id)}")
      println(s"payload.headerId: ${Algos.encode(b.payload.headerId)}")
    }
  }

  def checkFullBlockChainIsSynced(qty: Int): Unit = (0 until qty).foreach(_ => expectMsg(timeout, FullBlockChainIsSynced()))

  val dir: File = FileHelper.getRandomTempDir
  val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))

  val nodeViewHolder = TestProbe()
  val influx = TestProbe()

  val timeout: FiniteDuration = 10 seconds

    "HistoryApplicator add locall generated block" should {
      "chain sync" in {

        val blockQty = 10

        val initialHistory: History = generateDummyHistory(settings)
        val (genesisState, state, chain) = generateChain(blockQty)

        val historyApplicator: TestActorRef[HistoryApplicator] =
          TestActorRef[HistoryApplicator](
            HistoryApplicator.props(initialHistory, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
          )

        system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

        chain.foreach(historyApplicator ! LocallyGeneratedBlock(_))

        checkFullBlockChainIsSynced((blockQty + 1) * 2)
      }
    }

  "HistoryApplicator" should {
    "check queues" in {

      val initialHistory: History = generateDummyHistory(settings)
      val (genesisState, state, chain) = generateChain(1)

      val modifiers: Seq[PersistentModifier] = chain.flatMap(blockToModifiers)
      val modifierIds = modifiers.map(_.id)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(initialHistory, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      modifiers.foreach(historyApplicator ! ModifierFromRemote(_))

      println(s"modifiersQueue: ${historyApplicator.underlyingActor.modifiersQueue.size}")
      println(s"currentNumberOfAppliedModifiers: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")

      val modifiersQueue = historyApplicator.underlyingActor.modifiersQueue
      modifiersQueue.size shouldBe 2
      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 2

      historyApplicator ! StateApplicator.RequestNextModifier

      historyApplicator.underlyingActor.modifiersQueue shouldBe 0
      historyApplicator.underlyingActor.currentNumberOfAppliedModifiers shouldBe 2

      val history = historyApplicator.underlyingActor.history

      expectMsgType[StartModifiersApplicationOnStateApplicator](timeout)

      assert(modifierIds.forall(history.isModifierDefined))
      assert(modifierIds.map(Algos.encode) == modifiersQueue.map(_._1))
    }
  }

  "HistoryApplicator" should {
    "check queue for rollback limit" in {

      val overQty = 30

      val history: History = generateDummyHistory(settings)
      val (genesisState, state, chain) = generateChain(settings.levelDB.maxVersions + overQty)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )
      system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

      chain
        .flatMap(blockToModifiers)
        .foreach(historyApplicator ! ModifierFromRemote(_))

      println(s"modifiersQueue.size: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")
      historyApplicator.underlyingActor.modifiersQueue.size should be <= settings.levelDB.maxVersions

      receiveN((settings.levelDB.maxVersions + overQty) * 2, 30 seconds)

      history.getBestBlockHeight shouldBe settings.levelDB.maxVersions + overQty
    }
  }

  "HistoryApplicator" should {
    "check rollback for invalid state" in {

      val history: History = generateDummyHistory(settings)

      //generate invalid starting from 2 blocks
      val (genesisState, state, chain) = generateChain(3, Some(2))

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )
      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      chain.foreach(b => println(Algos.encode(b.header.id)))

      chain
        .flatMap(blockToModifiers)
        .foreach(historyApplicator ! ModifierFromRemote(_))

      checkFullBlockChainIsSynced(3 * 2)

      history.getBestBlockHeight shouldBe 1
      history.getBestBlock.map(b => Algos.encode(b.id)) shouldBe Some(Algos.encode(chain(1).id))
      history.getBestBlockHeightDB shouldBe 1
    }
  }
}
