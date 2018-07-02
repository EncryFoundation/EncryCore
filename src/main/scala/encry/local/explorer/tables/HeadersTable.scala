package encry.local.explorer.tables

import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.mempool.directive.TransferDirective
import scorex.crypto.encode.Base16

object HeadersTable {

  val name: String = "headers"
  val fields: Seq[String] = Seq(
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
  val fieldsString: String = fields.mkString("(", ", ", ")")

  def updateByIdSql(updateString: String): String = s"UPDATE $name SET $updateString WHERE id = ?"

  def dataString(b: EncryBlock): String = {
    val id: String = Base16.encode(b.id)
    val parentId: String = Base16.encode(b.parentId)
    val proofsRoot: String = Base16.encode(b.header.adProofsRoot)
    val stateRoot: String = Base16.encode(b.header.stateRoot)
    val transactionsRoot: String = Base16.encode(b.header.transactionsRoot)
    val proofs: String = b.adProofsOpt.map(p => Base16.encode(p.bytes)).getOrElse("")
    val solution: String = b.header.equihashSolution.ints.mkString("{", ", ", "}")
    val (minerAddress: String, minerReward: Long) = minerInfo(b.payload.transactions.last)
    val feesTotal: Long = b.payload.transactions.map(_.fee).sum
    val txsSize: Int = b.payload.transactions.map(_.bytes.length).sum

    s"('$id', '$parentId', '${b.header.version}', '${b.header.height}', '$proofsRoot', '$stateRoot', " +
      s"'$transactionsRoot', '${b.header.timestamp}', '${b.header.difficulty}', '${b.bytes.length}', '$solution', '$proofs', " +
      s"'${b.payload.transactions.size}', '$minerAddress', '$minerReward', '$feesTotal', '$txsSize', TRUE)"
  }

  private def minerInfo(coinbase: EncryBaseTransaction): (String, Long) = coinbase.directives.head match {
    case TransferDirective(address, amount, tokenIdOpt) if tokenIdOpt.isEmpty => address -> amount
    case _ => "unknown" -> 0
  }
}
