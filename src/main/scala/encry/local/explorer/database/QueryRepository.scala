package encry.local.explorer.database

import doobie.free.connection.ConnectionIO
import doobie.util.update.Update
import encry.ModifierId
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.HeaderDBVersion
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.crypto.encode.Base16
import cats.implicits._
import doobie.postgres.implicits._
import doobie.util.log.LogHandler
import encry.local.explorer.database.Tables.{HeadersTable, InputsTable, OutputsTable, TransactionsTable}
import encry.modifiers.mempool.{InputDBVersion, OutputDBVersion, TransactionDBVersion}

protected[database] object QueryRepository {

  def processBlockQuery(block: EncryBlock): ConnectionIO[Int] =
    for {
      headerR <- insertHeaderQuery(block)
      txsR    <- insertTransactionsQuery(block)
      outsR   <- insertOutputsQuery(block.payload)
      insR    <- insertInputsQuery(block.payload)
    } yield txsR + headerR + outsR + insR

  def markAsRemovedFromMainChainQuery(ids: List[ModifierId]): ConnectionIO[Int] = {
    val query = "UPDATE ${HeadersTable.name} SET best_chain = FALSE WHERE id = ?"
    Update[String](query).updateMany(ids.map(Base16.encode))
  }

  def insertHeaderQuery(block: EncryBlock): ConnectionIO[Int] = {
    val headerDB: HeaderDBVersion = HeadersTable.header(block)
    val query =
      """
        |INSERT INTO public.headers (id, parent_id, version, height, ad_proofs_root, state_root, transactions_root, ts, difficulty,
        |      block_size, equihash_solution, ad_proofs, tx_qty, miner_address, miner_reward, fees_total, txs_size, best_chain)
        |VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      """.stripMargin
    Update[HeaderDBVersion](query).run(headerDB)
  }

  // internal

  private implicit val han: LogHandler = LogHandler.jdkLogHandler

  private def insertTransactionsQuery(block: EncryBlock): ConnectionIO[Int] = {
    val txs: Seq[TransactionDBVersion] = TransactionsTable.txs(block)
    val query =
      """
        |INSERT INTO public.transactions (id, block_id, is_coinbase, ts)
        |VALUES (?, ?, ?, ?);
      """.stripMargin
    Update[TransactionDBVersion](query).updateMany(txs.toList)
  }

  private def insertInputsQuery(p: EncryBlockPayload): ConnectionIO[Int] = {
    val inputs = InputsTable.inputs(p)
    val query =
      """
        |INSERT INTO public.inputs (id, tx_id, serialized_proofs)
        |VALUES (?, ?, ?);
      """.stripMargin
    Update[InputDBVersion](query).updateMany(inputs.toList)
  }

  private def insertOutputsQuery(p: EncryBlockPayload): ConnectionIO[Int] = {
    val outputs: Seq[OutputDBVersion] = OutputsTable.outputs(p)
    val query =
      """
        |INSERT INTO public.outputs (id, tx_id, monetary_value, coin_id, contract_hash, data)
        |VALUES (?, ?, ?, ?, ?, ?);
        |""".stripMargin
    Update[OutputDBVersion](query).updateMany(outputs.toList)
  }
}
