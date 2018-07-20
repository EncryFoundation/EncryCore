package encry.local.explorer.database

import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, HeaderDBVersion}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.{InputDBVersion, OutputDBVersion, TransactionDBVersion}

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

    def header(b: EncryBlock): HeaderDBVersion = HeaderDBVersion(b)

  }

  final case object InputsTable extends Table {
    val name: String = "inputs"
    val fieldNames: Seq[String] = Seq("id", "tx_id", "serialized_proofs")

    def inputs(p: EncryBlockPayload): Seq[InputDBVersion] =
      p.transactions.flatMap(InputDBVersion(_))
  }

  final case object OutputsTable extends Table {
    val name: String = "outputs"
    val fieldNames: Seq[String] = Seq("id", "tx_id", "monetary_value", "coin_id", "contract_hash", "data")

    def outputs(p: EncryBlockPayload): Seq[OutputDBVersion] =
      p.transactions.flatMap(OutputDBVersion(_))
  }

  final case object TransactionsTable extends Table {
    val name: String = "transactions"
    val fieldNames: Seq[String] = Seq("id", "block_id", "is_coinbase", "ts")

    def txs(block: EncryBlock): Seq[TransactionDBVersion] = TransactionDBVersion(block)
  }

}
