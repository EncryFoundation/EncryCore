package encry.local.explorer.database

import doobie.free.connection.ConnectionIO
import doobie.util.update.Update
import encry.ModifierId
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, HeaderDBVersion}
import encry.modifiers.history.block.payload.EncryBlockPayload
import scorex.crypto.encode.Base16
import cats.implicits._
import doobie.postgres.implicits._
import doobie.util.log.LogHandler
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
    val query = s"UPDATE public.headers SET best_chain = FALSE WHERE id = ?"
    Update[String](query).updateMany(ids.map(Base16.encode))
  }

  def insertHeaderQuery(block: EncryBlock): ConnectionIO[Int] = {
    val headerDB: HeaderDBVersion = HeaderDBVersion(block)
    val query =
      """
        |INSERT INTO public.headers (id, parent_id, version, height, ad_proofs_root, state_root, transactions_root, ts, difficulty,
        |      block_size, equihash_solution, ad_proofs, tx_qty, miner_address, miner_reward, fees_total, txs_size, best_chain)
        |VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      """.stripMargin
    Update[HeaderDBVersion](query).run(headerDB)
  }

  def insertOrphanedHeaderQuery(header: EncryBlockHeader): ConnectionIO[Int] = {
    val headerDB: HeaderDBVersion = HeaderDBVersion(header)
    val query =
      """
        |INSERT INTO public.headers (id, parent_id, version, height, ad_proofs_root, state_root, transactions_root, ts, difficulty,
        |      block_size, equihash_solution, ad_proofs, tx_qty, miner_address, miner_reward, fees_total, txs_size, best_chain)
        |VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      """.stripMargin
    Update[HeaderDBVersion](query).run(headerDB)
  }

  private implicit val han: LogHandler = LogHandler.jdkLogHandler

  private def insertTransactionsQuery(block: EncryBlock): ConnectionIO[Int] = {
    val txs: Seq[TransactionDBVersion] = TransactionDBVersion(block)
    val query =
      """
        |INSERT INTO public.transactions (id, block_id, is_coinbase, ts)
        |VALUES (?, ?, ?, ?);
      """.stripMargin
    Update[TransactionDBVersion](query).updateMany(txs.toList)
  }

  private def insertInputsQuery(p: EncryBlockPayload): ConnectionIO[Int] = {
    val inputs: Seq[InputDBVersion] = p.transactions.flatMap(InputDBVersion(_))
    val query =
      """
        |INSERT INTO public.inputs (id, tx_id, serialized_proofs)
        |VALUES (?, ?, ?);
      """.stripMargin
    Update[InputDBVersion](query).updateMany(inputs.toList)
  }

  private def insertOutputsQuery(p: EncryBlockPayload): ConnectionIO[Int] = {
    val outputs: Seq[OutputDBVersion] = p.transactions.flatMap(OutputDBVersion(_))
    val query =
      """
        |INSERT INTO public.outputs (id, tx_id, monetary_value, coin_id, contract_hash, data)
        |VALUES (?, ?, ?, ?, ?, ?);
        |""".stripMargin
    Update[OutputDBVersion](query).updateMany(outputs.toList)
  }
}
