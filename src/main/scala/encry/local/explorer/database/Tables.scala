package encry.local.explorer.database

import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import scorex.crypto.encode.Base16

object Tables {

  sealed trait Table {
    def name: String
    def fieldNames: Seq[String]
    def allFields: String = fieldNames.mkString("(", ", ", ")")
  }

  final case object HeadersTable extends Table {
    val name: String = "headers"
    val fieldNames: Seq[String] = Seq(
      "id",
      "parent_id",
      "version",
      "height",
      "ad_proofs_root",
      "state_root",
      "transactions_root",
      "ts",
      "difficulty",
      "block_size",
      "equihash_solution",
      "ad_proofs",
      "tx_qty",
      "miner_address",
      "miner_reward",
      "fees_total",
      "txs_size",
      "best_chain"
    )

    def dataString(b: EncryBlock): String = b.dataString
    def dataString(h: EncryBlockHeader): String = h.dataString

  }

  final case object InputsTable extends Table {
    val name: String = "inputs"
    val fieldNames: Seq[String] = Seq("id", "tx_id", "serialized_proofs")

    def dataString(p: EncryBlockPayload): String =
      p.transactions.map(_.inputsString).mkString(", ")
  }

  final case object OutputsTable extends Table {
    val name: String = "outputs"
    val fieldNames: Seq[String] = Seq("id", "tx_id", "monetary_value", "coin_id", "contract_hash", "data")

    def dataString(p: EncryBlockPayload): String =
      p.transactions.map(_.outputsString).mkString(", ")
  }

  final case object TransactionsTable extends Table {
    val name: String = "transactions"
    val fieldNames: Seq[String] = Seq("id", "block_id", "is_coinbase", "ts")

    def dataStrings(h: EncryBlockHeader, p: EncryBlockPayload): String = {
      val coinbase: String = dataString(h, p.transactions.last, isCoinbase = true)
      val other: List[String] = p.transactions.init.map(t => dataString(h, t, isCoinbase = false)).toList
      (coinbase :: other).mkString(", ")
    }

    // internal

    private def dataString(h: EncryBlockHeader, tx: EncryBaseTransaction, isCoinbase: Boolean): String = {
      val id: String = Base16.encode(tx.id)
      val blockId: String = Base16.encode(h.id)
      val isCb: String = if (isCoinbase) "TRUE" else "FALSE"
      s"('$id', '$blockId', $isCb, ${h.timestamp})"
    }
  }

}
