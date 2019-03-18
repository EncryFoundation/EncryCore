package encry.view.mempool

import akka.actor.ActorSystem
import encry.modifiers.mempool.Transaction
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.{EncryGenerator, NetworkTimeProvider}
import org.scalatest.{Matchers, WordSpecLike}

class MempoolBloomFilter extends WordSpecLike with EncryGenerator with Matchers {

  val settings: EncryAppSettings = EncryAppSettings.read
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
  val as: ActorSystem = ActorSystem()

  val transactionsNumber: Int = 1000

  val mempool: Mempool = Mempool.empty(settings, timeProvider, as)
  val transactions: Seq[Transaction] = genValidPaymentTxs(transactionsNumber)

  transactions.foreach { tx =>
    val checkTx: Seq[ModifierId] = mempool.checkIfContains(Seq(tx.id))
    if (checkTx.nonEmpty) {
      mempool.putElementToBloomFilter(tx.id)
      mempool.put(tx)
    }
  }

  "Bloom filter" should {

    "allow to put new uniq transaction" in {
      mempool.unconfirmed.values.size shouldBe transactions.size
    }

    "not allow to put repeating transactions" in {
      transactions.foreach { tx =>
        val checkTx: Seq[ModifierId] = mempool.checkIfContains(Seq(tx.id))
        if (checkTx.nonEmpty) {
          mempool.putElementToBloomFilter(tx.id)
          mempool.put(tx)
        }
      }
      mempool.unconfirmed.values.size shouldBe transactions.size
    }

    "allow to put new uniq transactions into mempool with nonEmpty bloom filter" in {
      val uniqTransactions: Seq[Transaction] = genValidPaymentTxs(transactionsNumber)
      uniqTransactions.foreach { tx =>
        val checkTx: Seq[ModifierId] = mempool.checkIfContains(Seq(tx.id))
        if (checkTx.nonEmpty) {
          mempool.putElementToBloomFilter(tx.id)
          mempool.put(tx)
        }
      }
      mempool.unconfirmed.size shouldBe uniqTransactions.size + transactions.size
    }
  }
}