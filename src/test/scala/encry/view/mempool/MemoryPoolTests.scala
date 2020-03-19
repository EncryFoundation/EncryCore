package encry.view.mempool

import java.net.InetSocketAddress

import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.mpg.MemoryPool.{RolledBackTransactions, TransactionProcessing, UpdateMempoolReader}
import encry.mpg.{IntermediaryMempool, MemoryPool, MemoryPoolProcessor, MemoryPoolReader, MemoryPoolStorage, TransactionsValidator}
import encry.network.BlackList.BanReason.SemanticallyInvalidPersistentModifier
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DeliveryManagerTests.DMUtils.{createPeer, generateBlocks}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.settings.TestNetSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer, Payload}
import org.encryfoundation.common.network.BasicMessagesRepo.ModifiersNetworkMessage
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class MemoryPoolTests
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with BeforeAndAfterAll
    with OneInstancePerTest
    with TestNetSettings
    with StrictLogging {

  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(testNetSettings.ntp)

  "MemoryPool" should {
    "add new unique transactions" in {
      val mempool                = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions           = genValidPaymentTxs(10)
      val (newMempool, validTxs) = mempool.validateTransactions(transactions)
      newMempool.size shouldBe 10
      validTxs.map(_.encodedId).forall(transactions.map(_.encodedId).contains) shouldBe true
    }
    "reject not unique transactions" in {
      val mempool                          = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions                     = genValidPaymentTxs(10)
      val (newMempool, validTxs)           = mempool.validateTransactions(transactions)
      val (newMempoolAgain, validTxsAgain) = newMempool.validateTransactions(validTxs)
      newMempoolAgain.size shouldBe 10
      validTxsAgain.size shouldBe 0
    }
    "mempoolMaxCapacity works correct" in {
      val mempool                = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions           = genValidPaymentTxs(11)
      val (newMempool, validTxs) = mempool.validateTransactions(transactions)
      newMempool.size shouldBe 10
      validTxs.size shouldBe 10
    }
    "getTransactionsForMiner works fine" in {
      val mempool         = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions    = (0 until 10).map(k => coinbaseAt(k))
      val (newMempool, _) = mempool.validateTransactions(transactions)
      val txs    = newMempool.getTransactionsForMiner
      txs.map(_.encodedId).forall(transactions.map(_.encodedId).contains) shouldBe true
      transactions.map(_.encodedId).forall(txs.map(_.encodedId).contains) shouldBe true
    }
    "chainSynced is true on FullBlockChainIsSynced" in {
      val mP = TestActorRef[MemoryPoolProcessor](MemoryPoolProcessor.props(settings, timeProvider))
      mP ! FullBlockChainIsSynced
      mP.underlyingActor.chainSynced shouldBe true
    }
    "storage changes on RolledBackTransactions" in {
      val fakeActor = TestProbe()
      val storage = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val memPool = TestActorRef[MemoryPool](MemoryPool.props(settings, timeProvider, Some(fakeActor.ref), fakeActor.ref))
      val txs = (0 until 10).map(k => coinbaseAt(k))
      memPool ! RolledBackTransactions(txs)
      assert(memPool.underlyingActor.memoryPool != storage)
      memPool.underlyingActor.memoryPool.size shouldBe txs.length
    }
    "TransactionProcessing" in {
      val mP = TestActorRef[MemoryPoolProcessor](MemoryPoolProcessor.props(settings, timeProvider))
      mP ! TransactionProcessing(true)
      mP.underlyingActor.canProcessTransactions shouldBe true
      mP ! TransactionProcessing(false)
      mP.underlyingActor.canProcessTransactions shouldBe false
    }

  }
}
