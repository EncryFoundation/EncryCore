package encry.view.mempool

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.scalalogging.StrictLogging
import encry.settings.TestNetSettings
import encry.utils.NetworkTimeProvider
import encry.utils.TestEntityGenerator.genValidPaymentTxs
import encry.view.mempool.MemoryPool.{NewTransaction, TransactionsForMiner}
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

import scala.concurrent.duration._

class MemoryPoolTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with OneInstancePerTest
  with TestNetSettings
  with StrictLogging {

  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(testNetSettings.ntp)

  "MemoryPool" should {
    "add new unique transactions" in {
      val mempool = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions = genValidPaymentTxs(10)
      val (newMempool, validTxs) = mempool.validateTransactions(transactions)
      newMempool.size shouldBe 10
      validTxs.map(_.encodedId).forall(transactions.map(_.encodedId).contains) shouldBe true
    }
    "reject not unique transactions" in {
      val mempool = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions = genValidPaymentTxs(10)
      val (newMempool, validTxs) = mempool.validateTransactions(transactions)
      val (newMempoolAgain, validTxsAgain) = newMempool.validateTransactions(validTxs)
      newMempoolAgain.size shouldBe 10
      validTxsAgain.size shouldBe 0
    }
    "mempoolMaxCapacity works correct" in {
      val mempool = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions = genValidPaymentTxs(11)
      val (newMempool, validTxs) = mempool.validateTransactions(transactions)
      newMempool.size shouldBe 10
      validTxs.size shouldBe 10
    }
    "getTransactionsForMiner works fine" in {
      val mempool = MemoryPoolStorage.empty(testNetSettings, timeProvider)
      val transactions = genValidPaymentTxs(10)
      val (newMempool, validTxs) = mempool.validateTransactions(transactions)
      val (uPool, txs) = newMempool.getTransactionsForMiner
      uPool.size shouldBe 0
      txs.map(_.encodedId).forall(transactions.map(_.encodedId).contains) shouldBe true
      transactions.map(_.encodedId).forall(txs.map(_.encodedId).contains) shouldBe true
    }
  }
  "Mempool actor" should {
    "send transactions to miner" in {
      val miner = TestProbe()
      val mempool: TestActorRef[MemoryPool] = TestActorRef[MemoryPool](
        MemoryPool.props(testNetSettings, timeProvider, miner.ref, Some(TestProbe().ref)))
      val transactions = genValidPaymentTxs(4)
      transactions.foreach(mempool ! NewTransaction(_))
      mempool.underlyingActor.memoryPool.size shouldBe 4
      logger.info(s"generated: ${transactions.map(_.encodedId)}")
      miner.expectMsg(20.seconds, TransactionsForMiner(transactions.toIndexedSeq))
    }
  }


}
