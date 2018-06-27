package encry.local.explorer.tables

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.Algos

object TransactionsTable {

  val name: String = "transactions"
  val fields: Seq[String] = Seq("id", "block_id", "is_coinbase", "ts")
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def dataString(h: EncryBlockHeader, tx: EncryBaseTransaction, isCoinbase: Boolean): String = {
    val id: String = Algos.encode(tx.id)
    val blockId: String = Algos.encode(h.id)
    val cb: String = if (isCoinbase) "TRUE" else "FALSE"
    s"('$id', '$blockId', $cb, ${h.timestamp})"
  }

  def dataStrings(h: EncryBlockHeader, p: EncryBlockPayload): String = {
    val coinbase: String = dataString(h, p.transactions.last, isCoinbase = true)
    val other: List[String] = p.transactions.init.map(t => dataString(h, t, isCoinbase = false)).toList
    (coinbase :: other).mkString(", ")
  }
}
