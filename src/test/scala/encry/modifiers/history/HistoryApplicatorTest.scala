package encry.modifiers.history

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.settings.{EncryAppSettings, Settings}
import encry.storage.VersionalStorage
import encry.utils.ChainGenerator._
import encry.utils.{FileHelper, Keys}
import encry.view.actors.HistoryApplicator
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.history.History
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.utils.HistoryGenerator.dummyHistory

import scala.collection.Seq
import scala.concurrent.duration._

class HistoryApplicatorTest extends TestKit(ActorSystem())
  with WordSpecLike
  with ImplicitSender
  with BeforeAndAfterAll
  with Matchers
  with OneInstancePerTest
  with Keys
  with Settings {

  override def afterAll: Unit = shutdown(system)

  val testSettings: EncryAppSettings = settings.copy(storage = settings.storage.copy(state = VersionalStorage.LevelDB))

  def blockToModifiers(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload)

  def rollbackTest(transQty: Int) {
    val history: History = dummyHistory(testSettings, withoutPow = true)
    //generate invalid blocks begining from 6 blocks
    val (initialState, state, chain) = genChain(privKey, dir, testSettings, 10, transQty, Some(6))

    val historyApplicator: TestActorRef[HistoryApplicator] =
      TestActorRef[HistoryApplicator](
        HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
      )
    system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

    chain
      .flatMap(blockToModifiers)
      .foreach(historyApplicator ! ModifierFromRemote(_))

    expectMsg(timeout, FullBlockChainIsSynced())

    Thread.sleep(1000 + transQty/2)

    awaitCond(history.getBestBlockHeight == 4, timeout)

    checkBestBlock(history, chain(4))
  }

  def checkBestBlock(history: History, expectBlock: Block) {
    history.getBestHeaderHeight shouldBe expectBlock.header.height
    history.getBestHeader.map(h => Algos.encode(h.id)) shouldBe Some(Algos.encode(expectBlock.header.id))
    history.getBestBlock.map(b => Algos.encode(b.id)) shouldBe Some(Algos.encode(expectBlock.id))
    history.getBestBlockHeightDB shouldBe expectBlock.header.height
  }

  val dir: File = FileHelper.getRandomTempDir
  val wallet: EncryWallet = EncryWallet.readOrGenerate(testSettings.copy(directory = dir.getAbsolutePath))

  val nodeViewHolder = TestProbe()
  val influx = TestProbe()

  val timeout: FiniteDuration = 30 seconds

  "HistoryApplicator" should {

    "apply locall blocks and check chain sync" in {

      val blockQty = 10
      val history: History = dummyHistory(testSettings, withoutPow = true)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, blockQty)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      chain.foreach(historyApplicator ! LocallyGeneratedBlock(_))

      expectMsg(timeout, FullBlockChainIsSynced())
      awaitCond(history.getBestBlockHeight == blockQty - 1, timeout)

      checkBestBlock(history, chain(blockQty - 1))
    }


    "apply remote blocks and check chain sync" in {

      val blockQty = 10
      val history: History = dummyHistory(testSettings, withoutPow = true)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, blockQty)

      val modifiers: Seq[PersistentModifier] = chain.flatMap(blockToModifiers)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      modifiers.foreach(historyApplicator ! ModifierFromRemote(_))

      expectMsg(timeout, FullBlockChainIsSynced())
      awaitCond(history.getBestBlockHeight == blockQty - 1, timeout)

      checkBestBlock(history, chain(blockQty - 1))
    }

    "apply remote blocks and check queue for rollback height" in {

      val overQty = 30

      val history: History = dummyHistory(testSettings, withoutPow = true)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, testSettings.levelDB.maxVersions + overQty, 10)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )
      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      chain
        .flatMap(blockToModifiers)
        .foreach(historyApplicator ! ModifierFromRemote(_))

      historyApplicator.underlyingActor.modifiersQueue.size should be <= testSettings.levelDB.maxVersions

      expectMsg(timeout, FullBlockChainIsSynced())
      awaitCond(history.getBestBlockHeight == testSettings.levelDB.maxVersions + overQty - 1)

      checkBestBlock(history, chain(testSettings.levelDB.maxVersions + overQty - 1))
    }

    "should sync and check history rollback on invalid block " in {
      rollbackTest(100)
    }

    "should sync and check history rollback on invalid block for fat blocks" in {
      rollbackTest(5000)
    }

    "sync when apply headers and payloads separately" in {
      val blockQty = 10
      val history: History = dummyHistory(testSettings, withoutPow = true)
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, blockQty)

      val headers: Seq[PersistentModifier] = chain.map(_.header)
      val payloads: Seq[PersistentModifier] = chain.map(_.payload)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      headers.foreach(historyApplicator ! ModifierFromRemote(_))

      awaitCond(history.getBestHeaderHeight == blockQty - 1, timeout)
      history.getBestHeaderHeight shouldBe blockQty - 1
      history.getBestHeader.map(h => Algos.encode(h.id)) shouldBe Some(Algos.encode(chain(blockQty - 1).header.id))

      payloads.foreach(historyApplicator ! ModifierFromRemote(_))

      expectMsg(timeout, FullBlockChainIsSynced())
      awaitCond(history.getBestBlockHeight == blockQty - 1, timeout)
      checkBestBlock(history, chain(blockQty - 1))
    }

    "sync and rollback headers for invalid payload" in {
      val blockQty = 10
      val history: History = dummyHistory(testSettings, withoutPow = true)
      //generate invalid blocks begining from 6 blocks
      val (initialState, state, chain) = genChain(privKey, dir, testSettings, blockQty, 100, Some(6))

      val headers: Seq[PersistentModifier] = chain.map(_.header)
      val payloads: Seq[PersistentModifier] = chain.map(_.payload)

      val historyApplicator: TestActorRef[HistoryApplicator] =
        TestActorRef[HistoryApplicator](
          HistoryApplicator.props(history, testSettings, initialState, wallet, nodeViewHolder.ref, Some(influx.ref))
        )

      system.eventStream.subscribe(self, classOf[FullBlockChainIsSynced])

      headers.foreach(historyApplicator ! ModifierFromRemote(_))

      awaitCond(history.getBestHeaderHeight == blockQty - 1, timeout, 500 millis,
        s"history.getBestHeaderHeight ${history.getBestHeaderHeight} expected ${blockQty - 1}")
      history.getBestHeaderHeight shouldBe blockQty - 1
      history.getBestHeader.map(h => Algos.encode(h.id)) shouldBe Some(Algos.encode(chain(blockQty - 1).header.id))

      payloads.foreach(historyApplicator ! ModifierFromRemote(_))

      expectMsg(timeout, FullBlockChainIsSynced())
      awaitCond(history.getBestBlockHeight == 4, timeout, 500 millis,
        s"history.getBestBlockHeight ${history.getBestBlockHeight} expected 4")
      checkBestBlock(history, chain(4))
    }

  }
}
