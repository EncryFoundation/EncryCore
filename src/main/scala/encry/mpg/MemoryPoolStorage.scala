package encry.mpg

import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos

final case class MemoryPoolStorage private (
  transactions: Map[String, Transaction],
  settings: EncryAppSettings,
  networkTimeProvider: NetworkTimeProvider
) {

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

  def validateTransaction(tx: Transaction): (MemoryPoolStorage, Option[Transaction]) =
    if (isValid(tx) && size + 1 <= settings.mempool.maxCapacity)
      (putTransaction(tx), Some(tx))
    else {
      val withoutExpiredPool: MemoryPoolStorage = filter(isExpired)
      if (withoutExpiredPool.size + 1 <= settings.mempool.maxCapacity) (putTransaction(tx), Some(tx))
      else (withoutExpiredPool, None)
    }

  def validateTransactions(txs: Seq[Transaction]): (MemoryPoolStorage, Seq[Transaction]) = {
    val validatedElems: Seq[Transaction] = txs.filter(isValid)
    if (size + validatedElems.size <= settings.mempool.maxCapacity)
      (putTransactions(validatedElems.map(tx => tx.encodedId -> tx)), validatedElems)
    else {
      val withoutExpiredPool: MemoryPoolStorage = filter(isExpired)
      val transactionsWhichCanBeAdded: Seq[Transaction] = validatedElems
        .sortBy(_.fee)
        .take(settings.mempool.maxCapacity - withoutExpiredPool.size)
      (putTransactions(transactionsWhichCanBeAdded.map(tx => tx.encodedId -> tx)), transactionsWhichCanBeAdded)
    }
  }

  def getTransactionsForMiner: Seq[Transaction] =
    transactions.toIndexedSeq.sortBy { case (_, tx) => tx.fee }
      .foldLeft(Seq.empty[Transaction], Set.empty[String]) {
        case ((validated, inputs), (_, transaction)) =>
          val transactionInputsIds: Set[String] = transaction.inputs.map(input => Algos.encode(input.boxId)).toSet
          if (transactionInputsIds.size == transaction.inputs.size && transactionInputsIds.forall(
                id => !inputs.contains(id)
              ))
            (validated :+ transaction, inputs ++ transactionInputsIds)
          else (validated, inputs)
      }
      ._1

  def compareWithMod(block: Block): MemoryPoolStorage = removeSeveral(block.payload.txs.map(_.encodedId))

  def isValid: Transaction => Boolean = tx => tx.semanticValidity.isSuccess && !contains(tx.encodedId)

  def isExpired: (String, Transaction) => Boolean =
    (_, tx) => (networkTimeProvider.estimatedTime - tx.timestamp) < settings.mempool.utxMaxAge.toMillis

}

object MemoryPoolStorage {

  def empty(settings: EncryAppSettings, networkTimeProvider: NetworkTimeProvider): MemoryPoolStorage =
    MemoryPoolStorage(Map.empty[String, Transaction], settings, networkTimeProvider)
}
