package encry.view.mempool

import akka.actor.ActorSystem
import akka.testkit.{ TestActorRef, TestProbe }
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.mpg.MemoryPool._
import encry.mpg.{ MemoryPool, MemoryPoolStorage }
import encry.settings.TestNetSettings
import encry.utils.NetworkTimeProvider
import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }

import scala.concurrent.duration._

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
  }
}
