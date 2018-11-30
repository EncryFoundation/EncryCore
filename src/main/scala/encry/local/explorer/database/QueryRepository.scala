package encry.local.explorer.database

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import doobie.free.connection.ConnectionIO
import doobie.util.update.Update
import doobie.postgres.implicits._
import doobie.implicits._
import doobie.util.log.{ExecFailure, LogHandler, ProcessingFailure, Success}
import encry.EncryApp
import encry.modifiers.history.{Block, Header, HeaderDBVersion, Payload}
import encry.modifiers.mempool.directive.DirectiveDBVersion
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.{InputDBVersion, OutputDBVersion, Transaction, TransactionDBVersion}
import scorex.crypto.encode.Base16
import encry.EncryApp.settings
import encry.settings.EncryAppSettings

protected[database] object QueryRepository extends StrictLogging {

  def processBlockQuery(block: Block): ConnectionIO[Int] =
    for {
      _       <- markAsRemovedFromMainChainQuery(block.header.height)
      headerR <- insertHeaderQuery(block)
      txsR    <- insertTransactionsQuery(block)
      dirR    <- insertDirectivesQuery(block.payload.transactions)
      outsR   <- insertOutputsQuery(block.payload)
      insR    <- insertInputsQuery(block.payload)
    } yield txsR + headerR + outsR + insR + dirR

  def markAsRemovedFromMainChainQuery(ids: List[ModifierId]): ConnectionIO[Int] = {
    val query: String = s"UPDATE public.headers SET best_chain = FALSE WHERE id = ?"
    Update[String](query).updateMany(ids.map(Base16.encode))
  }

  def markAsRemovedFromMainChainQuery(height: Int): ConnectionIO[Int] = {
    val query: String = s"UPDATE public.headers SET best_chain = FALSE WHERE height = ?"
    Update[Int](query).run(height)
  }

  def insertHeaderQuery(block: Block): ConnectionIO[Int] = {
    val headerDB: HeaderDBVersion = HeaderDBVersion(block)
    val query: String =
      """
        |INSERT INTO public.headers (id, parent_id, version, height, ad_proofs_root, state_root, transactions_root, ts, nonce, difficulty,
        |      block_size, equihash_solution, ad_proofs, tx_qty, miner_address, miner_reward, fees_total, txs_size, best_chain, block_ad_proofs)
        |VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(id) DO UPDATE SET
        |best_chain = TRUE, ad_proofs = EXCLUDED.ad_proofs, tx_qty = EXCLUDED.tx_qty, miner_address = EXCLUDED.miner_address,
        |miner_reward = EXCLUDED.miner_reward, fees_total = EXCLUDED.fees_total, txs_size = EXCLUDED.txs_size, block_ad_proofs = EXCLUDED.block_ad_proofs
      """.stripMargin
    Update[HeaderDBVersion](query).run(headerDB)
  }

  def insertOrphanedHeaderQuery(header: Header): ConnectionIO[Int] = {
    val headerDB: HeaderDBVersion = HeaderDBVersion(header)
    val query: String =
      """
        |INSERT INTO public.headers (id, parent_id, version, height, ad_proofs_root, state_root, transactions_root, ts, nonce, difficulty,
        |      block_size, equihash_solution, ad_proofs, tx_qty, miner_address, miner_reward, fees_total, txs_size, best_chain, block_ad_proofs)
        |VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING
      """.stripMargin
    Update[HeaderDBVersion](query).run(headerDB)
  }

  def heightOptQuery: ConnectionIO[Option[Int]] =
    sql"SELECT MAX(height) FROM headers WHERE id IN (SELECT DISTINCT block_id FROM transactions);".query[Option[Int]].unique

  def headersByRangeQuery(from: Int, to: Int): ConnectionIO[List[HeaderDBVersion]] =
    sql"""SELECT id, parent_id, version, height, ad_proofs_root, state_root, transactions_root, ts, nonce, difficulty,
      block_size, equihash_solution, ad_proofs, tx_qty, miner_address, miner_reward, fees_total, txs_size, best_chain, block_ad_proofs
      FROM public.headers WHERE height >= $from AND height <= $to AND best_chain = TRUE ORDER BY height ASC;""".query[HeaderDBVersion].to[List]

  def txsByRangeQuery(from: Int, to: Int): ConnectionIO[List[TransactionDBVersion]] =
    sql"""SELECT id, number_in_block, fee, block_id, is_coinbase, ts, proof FROM public.transactions
         |WHERE block_id in (SELECT id FROM public.headers WHERE height >= $from AND height <= $to);
       """.stripMargin.query[TransactionDBVersion].to[List]

  def inputsByTransactionIdsQuery(ids: Seq[String]): ConnectionIO[List[InputDBVersion]] =
    sql"""WITH tmp(k) AS (VALUES (${ids.toList}))
         |SELECT * FROM public.inputs WHERE tx_id = ANY(SELECT unnest(k) FROM tmp)
       """.stripMargin.query[InputDBVersion].to[List]

  def directivesByTransactionIdsQuery(ids: Seq[String]): ConnectionIO[List[DirectiveDBVersion]] =
    sql"""WITH tmp(k) AS (VALUES (${ids.toList}))
         |SELECT * FROM public.directives WHERE tx_id = ANY(SELECT unnest(k) FROM tmp)
       """.stripMargin.query[DirectiveDBVersion].to[List]

  private def insertTransactionsQuery(block: Block): ConnectionIO[Int] = {
    val txs: Seq[TransactionDBVersion] = TransactionDBVersion(block)
    val query: String =
      """
        |INSERT INTO public.transactions (id, number_in_block, fee, block_id, is_coinbase, ts, proof)
        |VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING;
        |""".stripMargin
    Update[TransactionDBVersion](query).updateMany(txs.toList)
  }

  private def insertInputsQuery(p: Payload): ConnectionIO[Int] = {
    val inputs: Seq[InputDBVersion] = p.transactions.flatMap(InputDBVersion(_))
    val query: String =
      """
        |INSERT INTO public.inputs (id, tx_id, contract_bytes, serialized_proofs, number_in_tx)
        |VALUES (?, ?, ?, ?, ?) ON CONFLICT DO NOTHING;
        |""".stripMargin
    Update[InputDBVersion](query).updateMany(inputs.toList)
  }

  private def insertOutputsQuery(p: Payload): ConnectionIO[Int] = {
    val outputs: Seq[OutputDBVersion] = p.transactions.flatMap(OutputDBVersion(_))
    val query: String =
      """
        |INSERT INTO public.outputs (id, tx_id, monetary_value, coin_id, contract_hash, data)
        |VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING;
        |""".stripMargin
    Update[OutputDBVersion](query).updateMany(outputs.toList)
  }

  private def insertDirectivesQuery(txs: Seq[Transaction]): ConnectionIO[Int] = {
    val directives: Seq[DirectiveDBVersion] = txs.map(tx => tx.id -> tx.directives).flatMap {
      case (id, directives) => directives.zipWithIndex.map {
        case (directive, number) => directive.toDbVersion(id, number)
      }
    }
    val query: String =
      """
        |INSERT INTO public.directives (tx_id, number_in_tx, type_id, is_valid, contract_hash, amount, address, token_id_opt, data_field)
        |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING;
        |""".stripMargin
    Update[DirectiveDBVersion](query).updateMany(directives.toList)
  }

  private implicit val logHandler: LogHandler = LogHandler {
    case Success(s, a, e1, e2) =>
      logger.info(s"""Successful Statement Execution:
                  |
            |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                  |
            | arguments = [${a.mkString(", ")}]
                  |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (${(e1 + e2).toMillis} ms total)
          """.stripMargin)
    case ProcessingFailure(s, a, e1, e2, t) =>
      logger.warn(s"""Failed Resultset Processing:
                  |
            |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                  |
            | arguments = [${a.mkString(", ")}]
                  |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (failed) (${(e1 + e2).toMillis} ms total)
                  |   failure = ${t.getMessage}
          """.stripMargin)
    case ExecFailure(s, a, e1, t) =>
      logger.warn(s"""Failed Statement Execution:
                  |
            |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                  |
            | arguments = [${a.mkString(", ")}]
                  |   elapsed = ${e1.toMillis} ms exec (failed)
                  |   failure = ${t.getMessage}
          """.stripMargin)
  }
}
