package encry.local.explorer.database

import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import doobie.hikari.HikariTransactor
import QueryRepository._
import com.typesafe.scalalogging.StrictLogging
import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.implicits._
import encry.EncryApp.settings
import encry.modifiers.history.{Block, Header}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.history.HeaderDBVersion
import encry.modifiers.mempool.directive.DirectiveDBVersion
import encry.modifiers.mempool.{InputDBVersion, TransactionDBVersion}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

class DBService extends StrictLogging {

  def processBlock(block: Block): Future[Int] = runAsync(processBlockQuery(block), "processBlock")
    .map { count =>
      logger.info(s"Successfully wrote block on height ${block.header.height} as best chain")
      count
    }


  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int] =
    runAsync(markAsRemovedFromMainChainQuery(ids), "markAsRemovedFromMainChain")

  def processOrphanedHeader(header: Header): Future[Int] =
    runAsync(insertOrphanedHeaderQuery(header), "processOrphanedHeader")

  def selectHeightOpt: Future[Option[Int]] = runAsync(heightOptQuery, "selectHeightOpt")

  def headersByRange(from: Int, to: Int): Future[List[HeaderDBVersion]] =
    runAsync(headersByRangeQuery(from, to), "headersByRange")

  def txsByRange(from: Int, to: Int): Future[List[TransactionDBVersion]] =
    runAsync(txsByRangeQuery(from, to), "txsByRange")

  def directivesByTxIds(ids: Seq[String]): Future[List[DirectiveDBVersion]] =
    runAsync(directivesByTransactionIdsQuery(ids), "directivesByTxIds")

  def inputsByTxIds(ids: Seq[String]): Future[List[InputDBVersion]] =
    runAsync(inputsByTransactionIdsQuery(ids), "inputsByTxIds")

  private lazy val dataSource = new HikariDataSource
  if (settings.postgres.exists(_.enableSave)) {
    dataSource.setJdbcUrl(settings.postgres.map(_.host + "?loggerLevel=OFF").getOrElse(throw new RuntimeException("host not specified")))
    dataSource.setUsername(settings.postgres.map(_.user).getOrElse(throw new RuntimeException("user not specified")))
    dataSource.setPassword(settings.postgres.map(_.password).getOrElse(throw new RuntimeException("password not specified")))
    dataSource.setMaximumPoolSize(settings.postgres.map(_.maxPoolSize).getOrElse(1))
  }

  private lazy val pgTransactor: HikariTransactor[IO] = HikariTransactor[IO](dataSource)

  def shutdown(): Future[Unit] = {
    logger.info("Shutting down dbService")
    pgTransactor.shutdown.unsafeToFuture
  }

  private def runAsync[A](io: ConnectionIO[A], queryName: String): Future[A] =
    (for {
      res <- io.transact(pgTransactor)
    } yield res)
      .unsafeToFuture()
      .recoverWith {
        case NonFatal(th) =>
          logger.warn(s"Failed to perform $queryName query with exception ${th.getLocalizedMessage}")
          Future.failed(th)
      }
}

object DBService {
  def apply(): DBService = new DBService
}
