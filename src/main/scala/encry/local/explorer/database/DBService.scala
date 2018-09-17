package encry.local.explorer.database

import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import doobie.hikari.HikariTransactor
import QueryRepository._
import com.zaxxer.hikari.HikariDataSource
import encry.EncryApp.settings
import encry.modifiers.history.{Block, Header, HeaderDBVersion}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.history.HeaderDBVersion
import encry.modifiers.mempool.directive.DirectiveDBVersion
import encry.modifiers.mempool.{InputDBVersion, TransactionDBVersion}
import encry.utils.Logging

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

class DBService extends Logging {

  def processBlock(block: Block): Future[Int] = runAsync(processBlockQuery(block))

  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int] = runAsync(markAsRemovedFromMainChainQuery(ids))

  def processOrphanedHeader(header: Header): Future[Int] = runAsync(insertOrphanedHeaderQuery(header))

  def selectHeight: Future[Int] = runAsync(heightQuery)

  def headersByRange(from: Int, to: Int): Future[List[HeaderDBVersion]] = runAsync(headersByRangeQuery(from, to))

  def txsByRange(from: Int, to: Int): Future[List[TransactionDBVersion]] = runAsync(txsByRangeQuery(from, to))

  def directivesByTxIds(ids: Seq[String]): Future[List[DirectiveDBVersion]] = runAsync(directivesByTransactionIdsQuery(ids))

  def inputsByTxIds(ids: Seq[String]): Future[List[InputDBVersion]] = runAsync(inputsByTransactionIdsQuery(ids))

  private lazy val dataSource = new HikariDataSource
  if (settings.postgres.exists(_.enableSave) || settings.postgres.exists(_.enableRestore)) {
    dataSource.setJdbcUrl(settings.postgres.map(_.host).getOrElse(throw new RuntimeException("host not specified")))
    dataSource.setUsername(settings.postgres.map(_.user).getOrElse(throw new RuntimeException("user not specified")))
    dataSource.setPassword(settings.postgres.map(_.password).getOrElse(throw new RuntimeException("password not specified")))
    dataSource.setMaximumPoolSize(5)
  }

  private lazy val pgTransactor: HikariTransactor[IO] = HikariTransactor[IO](dataSource)

  private def runAsync[A](io: ConnectionIO[A]): Future[A] =
    (for {
      res <- io.transact(pgTransactor)
    } yield res)
      .unsafeToFuture()
      .recoverWith {
        case NonFatal(th) =>
          logWarn(s"Failed to perform db operation with exception ${th.getLocalizedMessage}")
          Future.failed(th)
      }
}

object DBService {
  def apply(): DBService = new DBService
}
