package encry.view.mempool

import encry.modifiers.mempool.EncryTransaction
import encry.settings.EncryAppSettings
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}
import scorex.core.utils.NetworkTimeProvider

class EncryMempoolSpec extends PropSpec with Matchers with EncryGenerator {

  lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

  lazy val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(encrySettings.scorexSettings.ntp)

  property("Mempool.put(txs) should not allow overflow.") {

    val mempool: EncryMempool = EncryMempool.empty()

    val maxCapacity: Int = encrySettings.nodeSettings.mempoolMaxCapacity

    val txs: Seq[EncryTransaction] = genValidPaymentTxs(maxCapacity + 12)

    mempool.put(txs)

    mempool.size shouldEqual maxCapacity
  }

  property("Mempool should not accept invalid transactions.") {

    val mempool: EncryMempool = EncryMempool.empty()

    val validTxs: Seq[EncryTransaction] = genValidPaymentTxs(60)

    val invalidTxs: Seq[EncryTransaction] = genInvalidPaymentTxs(100)

    mempool.put(validTxs ++ invalidTxs)

    mempool.size shouldEqual validTxs.size
  }
}
