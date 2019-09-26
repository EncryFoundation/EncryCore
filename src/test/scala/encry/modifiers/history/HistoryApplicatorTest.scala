package encry.modifiers.history

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.{EncryAppSettings, Settings}
import encry.storage.VersionalStorage
import encry.utils.ChainGenerator._
import encry.utils.FileHelper
import encry.view.actors.HistoryApplicator
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.history.History
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

import scala.collection.Seq
import scala.concurrent.duration._

class HistoryApplicatorTest extends TestKit(ActorSystem())  with WordSpecLike  with ImplicitSender  with BeforeAndAfterAll  with Matchers
  with InstanceFactory with OneInstancePerTest with Settings {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }
  val testSettings: EncryAppSettings = settings.copy(storage = settings.storage.copy(state = VersionalStorage.LevelDB))

  def blockToModifiers(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload)

  def rollbackTest(transQty: Int) {
    val history: History = generateDummyHistory(testSettings)
    //generate invalid blocks begining from 6 blocks
    val (initialState, state, chain) = genChain(privKey, dir, testSettings, 10, transQty, Some(6))

    val historyApplicator: TestActorRef[HistoryApplicator] =
      TestActorRef[HistoryApplicator](
        HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
      )
    system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

    chain
      .flatMap(blockToModifiers)
      .foreach(historyApplicator ! ModifierFromRemote(_))

    Thread.sleep(3000 + transQty/30)
    //receiveN(6 * 2, 120 seconds)

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

  def checkFullBlockChainIsSynced(qty: Int): Unit = (0 until qty).foreach(_ => expectMsg(timeout, FullBlockChainIsSynced()))

  val dir: File = FileHelper.getRandomTempDir
  val wallet: EncryWallet = EncryWallet.readOrGenerate(testSettings.copy(directory = dir.getAbsolutePath))

  val nodeViewHolder = TestProbe()
  val influx = TestProbe()

  val timeout: FiniteDuration = 10 seconds

  "HistoryApplicator" should {

    "apply locall blocks and check chain sync" in {

      val blockQty = 10
      val history: History = generateDummyHistory(testSettings)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, blockQty)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      chain.foreach(historyApplicator ! LocallyGeneratedBlock(_))

      expectMsg(timeout, FullBlockChainIsSynced())

      history.getBestBlockHeight shouldBe blockQty - 1
    }

    "apply remote blocks and check chain sync" in {

      val blockQty = 10
      val history: History = generateDummyHistory(testSettings)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, blockQty)

      val modifiers: Seq[PersistentModifier] = chain.flatMap(blockToModifiers)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      modifiers.foreach(historyApplicator ! ModifierFromRemote(_))

      Thread.sleep(3000)

      expectMsg(timeout, FullBlockChainIsSynced())
      history.getBestBlockHeight shouldBe blockQty - 1
    }

    "apply remote blocks and check queue for rollback height" in {

      val overQty = 30

      val history: History = generateDummyHistory(testSettings)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, testSettings.levelDB.maxVersions + overQty, 10)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )
      system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

      chain
        .flatMap(blockToModifiers)
        .foreach(historyApplicator ! ModifierFromRemote(_))

      historyApplicator.underlyingActor.modifiersQueue.size should be <= testSettings.levelDB.maxVersions

      Thread.sleep(5000)
      //receiveN((testSettings.levelDB.maxVersions + overQty) * 2, 120 seconds)

      history.getBestBlockHeight shouldBe testSettings.levelDB.maxVersions + overQty - 1
    }

    "apply remote blocks and check history rollback for invalid state" in {
      rollbackTest(100)
    }

    "apply remote blocks and check history rollback on fat blocks" in {
      rollbackTest(5000)
    }

  }
}
