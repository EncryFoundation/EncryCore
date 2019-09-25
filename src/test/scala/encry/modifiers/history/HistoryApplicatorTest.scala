package encry.modifiers.history

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.TransactionFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Settings
import encry.storage.VersionalStorage
import encry.utils.ChainUtils.{coinbaseTransaction, defaultPaymentTransactionScratch, privKey, _}
import encry.utils.{ChainUtils, FileHelper}
import encry.view.actors.HistoryApplicator
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.history.History
import encry.view.state.{BoxHolder, UtxoState}
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.{Pay2PubKeyAddress, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryProposition}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height}
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scorex.crypto.signatures.PublicKey
import scorex.utils.{Random => ScorexRandom}

import scala.util.Random
import scala.collection.Seq
import scala.concurrent.duration._

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

  def blockToModifiers(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload)

  def rndAddress: Address = Pay2PubKeyAddress(PublicKey @@ ScorexRandom.randomBytes()).address

  def genChain(blockQty: Int, transPerBlock: Int = 1, genInvalidBlockFrom: Option[Int] = None): (UtxoState, UtxoState, List[Block]) = {

    def genForkBlock(boxQty: Int, prevBlock: Block, box: AssetBox, addDiff: Difficulty = Difficulty @@ BigInt(0)): (Block, Seq[AssetBox]) = {
      val timestamp = System.currentTimeMillis()

      val transactions: Seq[Transaction] = Seq(
        ChainUtils.defaultPaymentTransactionScratch(privKey, 1L, timestamp, IndexedSeq(box), rndAddress, 1000L, None, boxQty),
        ChainUtils.coinbaseTransaction(prevBlock.header.height + 1)
      )

      val header = Header(1.toByte, prevBlock.id, Payload.rootHash(transactions.map(_.id)), timestamp,
        prevBlock.header.height + 1, Random.nextLong(), Difficulty @@ (BigInt(1) + addDiff), EquihashSolution(Seq(1, 3)))

      (Block(header, Payload(header.id, transactions)), transactions.head.newBoxes.tail.map(_.asInstanceOf[AssetBox]).toSeq)
    }

    def genNextBlockForState(prevBlock: Block, state: UtxoState, boxes: Seq[AssetBox], invalid: Boolean = false,
                             addDiff: Difficulty = Difficulty @@ BigInt(0)): (Block, Seq[AssetBox]) = {
      val timestamp = System.currentTimeMillis()

      val amount = if(invalid) boxes.map(_.amount).sum + 1L else 1L

      val transactions: Seq[Transaction] =
        boxes.map(b => TransactionFactory.defaultPaymentTransactionScratch(privKey, 1L, timestamp, IndexedSeq(b), rndAddress, amount)) :+
          ChainUtils.coinbaseTransaction(prevBlock.header.height + 1)

      val header = Header(1.toByte, prevBlock.id, Payload.rootHash(transactions.map(_.id)), timestamp,
        prevBlock.header.height + 1, Random.nextLong(), Difficulty @@ (BigInt(1) + addDiff), EquihashSolution(Seq(1, 3)))

      val newBoxes = transactions.filter(_.newBoxes.size > 1).map(_.newBoxes.toSeq(1).asInstanceOf[AssetBox])
      (Block(header, Payload(header.id, transactions)), newBoxes)
    }

    assert(blockQty >= 2, "chain at least 2 blocks")

    val genesisBlock: Block = ChainUtils.generateGenesisBlock(Height @@ 0)

    val state: UtxoState = utxoFromBoxHolder(BoxHolder(Seq.empty), dir, None, settings, VersionalStorage.LevelDB)
      .applyModifier(genesisBlock).right.get

    val (forkBlock, forkBoxes) = genForkBlock(transPerBlock, genesisBlock, genesisBlock.payload.txs.head.newBoxes.map(_.asInstanceOf[AssetBox]).head)
    val forkState = state.applyModifier(forkBlock).right.get

    val (chain, _, newState, _) =
      (2 until blockQty).foldLeft(List(genesisBlock, forkBlock), forkBlock, forkState, forkBoxes) {
        case ((blocks, blockL, stateL, boxes), height) =>
          val invalid = genInvalidBlockFrom.exists(height + 1 >= _)
          val (newBlock, newBoxes) = genNextBlockForState(blockL, stateL, boxes, invalid)
          val newState = if(invalid) stateL else stateL.applyModifier(newBlock).right.get
          (blocks :+ newBlock, newBlock, newState, newBoxes)
      }
    (state, newState, chain)
  }

  def rollbackTest(transQty: Int) {

    val history: History = generateDummyHistory(settings)
    //generate invalid blocks begining from 6 blocks
    val (genesisState, state, chain) = genChain(10, transQty, Some(6))

    val historyApplicator: TestActorRef[HistoryApplicator] =
      TestActorRef[HistoryApplicator](
        HistoryApplicator.props(history, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
      )
    system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

    chain
      .flatMap(blockToModifiers)
      .foreach(historyApplicator ! ModifierFromRemote(_))

    receiveN(6 * 2, 30 seconds)

    history.getBestBlockHeight shouldBe 4
    history.getBestBlock.map(b => Algos.encode(b.id)) shouldBe Some(Algos.encode(chain(4).id))
    history.getBestBlockHeightDB shouldBe 4
  }

//  def applyBlocksToHistory(history: History, blocks: Seq[Block]): History =
//    blocks.foldLeft(history) {
//      case (prevHistory, block) =>
//        prevHistory.append(block.header)
//        prevHistory.append(block.payload)
//        prevHistory.reportModifierIsValid(block)
//    }

//  def printIds(blocks: List[Block]) = {
//    blocks.foreach { b =>
//      println(s"header.timestamp: ${b.header.timestamp}")
//      println(s"header.id: ${Algos.encode(b.header.id)}")
//      println(s"header.payloadId: ${Algos.encode(b.header.payloadId)}")
//      println(s"payload.id: ${Algos.encode(b.payload.id)}")
//      println(s"payload.headerId: ${Algos.encode(b.payload.headerId)}")
//    }
//  }

  //def checkFullBlockChainIsSynced(qty: Int): Unit = (0 until qty).foreach(_ => expectMsg(timeout, FullBlockChainIsSynced()))

  val dir: File = FileHelper.getRandomTempDir
  val wallet: EncryWallet = EncryWallet.readOrGenerate(settings.copy(directory = dir.getAbsolutePath))

  val nodeViewHolder = TestProbe()
  val influx = TestProbe()

  val timeout: FiniteDuration = 10 seconds

  "HistoryApplicator" should {

    "apply locall blocks and chain sync" in {

      val blockQty = 10
      val history: History = generateDummyHistory(settings)
      val (genesisState, state, chain) = genChain(blockQty)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

      chain.foreach(historyApplicator ! LocallyGeneratedBlock(_))

      receiveN(blockQty * 2, 30 seconds)
      history.getBestBlockHeight shouldBe blockQty - 1
    }

    "check SemanticallySuccessfulModifier per block" in {

      val blockQty = 2
      val initialHistory: History = generateDummyHistory(settings)
      val (genesisState, state, chain) = genChain(blockQty)

      val modifiers: Seq[PersistentModifier] = chain.flatMap(blockToModifiers)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(initialHistory, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

      modifiers.foreach(historyApplicator ! ModifierFromRemote(_))

      //println(s"test.modifiersQueue: ${historyApplicator.underlyingActor.modifiersQueue.size}")
      //println(s"test.currentNumberOfAppliedModifiers: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")

      receiveN(blockQty * 2, timeout).forall { case _: SemanticallySuccessfulModifier => true }
    }

    "check queue for rollback height" in {

      val overQty = 30

      val history: History = generateDummyHistory(settings)
      val (genesisState, state, chain) = genChain(settings.levelDB.maxVersions + overQty, 10)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, settings, genesisState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )
      system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

      chain
        .flatMap(blockToModifiers)
        .foreach(historyApplicator ! ModifierFromRemote(_))

      //println(s"modifiersQueue.size: ${historyApplicator.underlyingActor.currentNumberOfAppliedModifiers}")
      historyApplicator.underlyingActor.modifiersQueue.size should be <= settings.levelDB.maxVersions

      receiveN((settings.levelDB.maxVersions + overQty) * 2, 30 seconds)

      history.getBestBlockHeight shouldBe settings.levelDB.maxVersions + overQty - 1
    }

    "check history rollback for invalid state" in {
      rollbackTest(100)
    }

    "check history rollback on fat blocks" in {
      rollbackTest(5000)
    }

  }
}
