package encry.local.explorer.database.tables

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import scorex.crypto.encode.Base16

object TransactionsTable {

  val name: String = "transactions"
  val fields: Seq[String] = Seq("id", "block_id", "is_coinbase", "ts")
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def dataString(h: EncryBlockHeader, tx: EncryBaseTransaction, isCoinbase: Boolean): String = {
    val id: String = Base16.encode(tx.id)
    val blockId: String = Base16.encode(h.id)
    val isCb: String = if (isCoinbase) "TRUE" else "FALSE"
    s"('$id', '$blockId', $isCb, ${h.timestamp})"
  }

  def dataStrings(h: EncryBlockHeader, p: EncryBlockPayload): String = {
    val coinbase: String = dataString(h, p.transactions.last, isCoinbase = true)
    val other: List[String] = p.transactions.init.map(t => dataString(h, t, isCoinbase = false)).toList
    (coinbase :: other).mkString(", ")
  }
}
