package encry.view.mempool

import encry.settings.EncryAppSettings
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}
import scorex.core.utils.NetworkTimeProvider

class EncryMempoolSpec extends PropSpec with Matchers with EncryGenerator {

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(encrySettings.scorexSettings.ntp)

  property("Mempool.put(txs) should not allow overflow.") {

    val mempool = EncryMempool.empty(encrySettings, timeProvider)

    val maxCapacity = encrySettings.nodeSettings.mempoolMaxCapacity

    val txs = genValidPaymentTxs(maxCapacity + 12)

    mempool.put(txs)

    mempool.size shouldEqual maxCapacity
  }

  property("Mempool should not accept invalid transactions.") {

    val mempool = EncryMempool.empty(encrySettings, timeProvider)

    val validTxs = genValidPaymentTxs(60)

    val invalidTxs = genInvalidPaymentTxs(100)

    mempool.put(validTxs ++ invalidTxs)

    mempool.size shouldEqual validTxs.size
  }
}
