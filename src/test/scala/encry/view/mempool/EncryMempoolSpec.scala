package encry.view.mempool

import encry.modifiers.mempool.Transaction
import akka.actor.ActorSystem
import encry.modifiers.mempool.EncryTransaction
import encry.settings.EncryAppSettings
import encry.utils.{EncryGenerator, NetworkTimeProvider}
import org.scalatest.{Matchers, PropSpec}

class EncryMempoolSpec extends PropSpec with Matchers with EncryGenerator {

  lazy val settings: EncryAppSettings = EncryAppSettings.read

  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  lazy val as: ActorSystem = ActorSystem()

  property("Mempool.put(txs) should not allow overflow.") {

    val mempool: EncryMempool = EncryMempool.empty(settings, timeProvider, as)

    val maxCapacity: Int = settings.node.mempoolMaxCapacity

    val txs: Seq[Transaction] = genValidPaymentTxs(maxCapacity + 12)

    mempool.put(txs)

    mempool.size shouldBe maxCapacity
  }

  property("Mempool should not accept invalid transactions.") {

    val mempool: EncryMempool = EncryMempool.empty(settings, timeProvider, as)

    val validTxs: Seq[Transaction] = genValidPaymentTxs(60)

    val invalidTxs: Seq[Transaction] = genInvalidPaymentTxs(100)

    mempool.put(validTxs ++ invalidTxs)

    mempool.size shouldEqual validTxs.size
  }
}
