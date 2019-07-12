package encry.view.mempool

import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import scala.collection.IndexedSeq

final case class MemoryPoolStorage private(transactions: Map[String, Transaction],
                                           settings: EncryAppSettings,
                                           networkTimeProvider: NetworkTimeProvider) {

  lazy val size: Int = transactions.size

  def get(elem: String): Option[Transaction] = transactions.get(elem)

  def contains(elem: String): Boolean = transactions.contains(elem)

  def filter(f: (String, Transaction) => Boolean): MemoryPoolStorage =
    MemoryPoolStorage(transactions.filter { case (a, b) => f(a, b) }, settings, networkTimeProvider)

  def putTransaction(transaction: Transaction): MemoryPoolStorage =
    MemoryPoolStorage(transactions.updated(transaction.encodedId, transaction), settings, networkTimeProvider)

  def putTransactions(elems: Seq[(String, Transaction)]): MemoryPoolStorage =
    MemoryPoolStorage(transactions ++ elems.toMap, settings, networkTimeProvider)

  def remove(id: String): MemoryPoolStorage =
    MemoryPoolStorage(transactions - id, settings, networkTimeProvider)

  def removeSeveral(ids: Seq[String]): MemoryPoolStorage =
    MemoryPoolStorage(transactions -- ids, settings, networkTimeProvider)

  def collect(p: (String, Transaction) => Boolean,
              f: (String, Transaction) => (String, Transaction)): MemoryPoolStorage =
    MemoryPoolStorage(transactions.collect { case (a, b) if p(a, b) => f(a, b) }, settings, networkTimeProvider)

  def validateTransactions(txs: Seq[Transaction]): (MemoryPoolStorage, Seq[Transaction]) = {
    val validatedElems: Seq[Transaction] = txs.filter(isValid)
    if (size + validatedElems.size <= settings.node.mempoolMaxCapacity)
      (putTransactions(validatedElems.map(tx => tx.encodedId -> tx)), validatedElems)
    else {
      val withoutExpiredPool: MemoryPoolStorage = filter(isExpired)
      val transactionsWhichCanBeAdded: Seq[Transaction] = validatedElems
        .sortBy(_.fee)
        .take(settings.node.mempoolMaxCapacity - withoutExpiredPool.size)
      (putTransactions(transactionsWhichCanBeAdded.map(tx => tx.encodedId -> tx)), transactionsWhichCanBeAdded)
    }
  }

  def getTransactionsForMiner: (MemoryPoolStorage, IndexedSeq[Transaction]) = {
    val (transactionsForMiner: IndexedSeq[Transaction], _) = transactions
      .toIndexedSeq
      .sortBy { case (_, tx) => tx.fee }
      .foldLeft(IndexedSeq.empty[Transaction], Set.empty[String]) {
        case ((validated, inputs), (_, transaction)) =>
          val transactionInputsIds: Set[String] = transaction.inputs.map(input => Algos.encode(input.boxId)).toSet
          if (transactionInputsIds.size == inputs.size && transactionInputsIds.forall(id => !inputs.contains(id)))
            (validated :+ transaction, inputs ++ transactionInputsIds)
          else (validated, inputs)
      }
    (removeSeveral(transactionsForMiner.map(_.encodedId)), transactionsForMiner)
  }

  def isValid: Transaction => Boolean = tx => tx.semanticValidity.isSuccess && contains(tx.encodedId)

  def isExpired: (String, Transaction) => Boolean =
    (_, tx) => (networkTimeProvider.estimatedTime - tx.timestamp) < settings.node.utxMaxAge.toMillis

}

object MemoryPoolStorage {

  def empty(settings: EncryAppSettings, networkTimeProvider: NetworkTimeProvider): MemoryPoolStorage =
    MemoryPoolStorage(Map.empty[String, Transaction], settings, networkTimeProvider)
}