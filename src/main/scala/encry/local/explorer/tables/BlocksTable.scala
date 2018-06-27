package encry.local.explorer.tables

import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.mempool.directive.TransferDirective
import encry.settings.Algos

object BlocksTable {

  val name: String = "blocks"
  val fields: Seq[String] = Seq(
    "id",
    "parent_id",
    "version",
    "height",
    "ad_proofs_root",
    "state_root",
    "transactions_root",
    "timestamp",
    "difficulty",
    "block_size",
    "equihash_solution",
    "ad_proofs",
    "txs_count",
    "miner_address",
    "miner_reward",
    "fees_total",
    "txs_size"
  )
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def dataString(b: EncryBlock): String = {
    val parentId: String = Algos.encode(b.parentId)
    val proofsRoot: String = Algos.encode(b.header.adProofsRoot)
    val stateRoot: String = Algos.encode(b.header.stateRoot)
    val transactionsRoot: String = Algos.encode(b.header.transactionsRoot)
    val proofs: String = b.adProofsOpt.fold("{}")(_.bytes.mkString("{", ", ", "}"))
    val solution: String = b.header.equihashSolution.ints.mkString("{", ", ", "}")
    val (minerAddress: String, minerRaward: Long) = minerInfo(b.payload.transactions.last)
    val feesTotal: Long = b.payload.transactions.map(_.fee).sum
    val txsSize: Int = b.payload.transactions.map(_.bytes.length).sum

    s"('$parentId', '${b.header.version}', '${b.header.version}', '${b.header.height}', '$proofsRoot', '$stateRoot', " +
      s"'$transactionsRoot', '${b.header.timestamp}', '${b.header.difficulty}', '${b.bytes.length}', '$solution', '$proofs', " +
      s"'${b.payload.transactions.size}', '$minerAddress', '$minerRaward', '$feesTotal', '$txsSize')"
  }

  private def minerInfo(coinbase: EncryBaseTransaction): (String, Long) = coinbase.directives.head match {
    case TransferDirective(address, amount, tokenIdOpt) if tokenIdOpt.isEmpty => address -> amount
    case _ => "unknown" -> 0
  }
}
