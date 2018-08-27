package encry.local.explorer.database

import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.implicits._
import encry.EncryApp.{settings, system}
import encry.ModifierId
import QueryRepository._
import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.HikariTransactor
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.stats.LoggingActor.LogMessage
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

class DBService {

  def processBlock(block: EncryBlock): Future[Int] = runAsync(processBlockQuery(block))

  def processHeader(block: EncryBlock): Future[Int] = runAsync(insertHeaderQuery(block))

  def markAsRemovedFromMainChain(ids: List[ModifierId]): Future[Int] = runAsync(markAsRemovedFromMainChainQuery(ids))

  def processOrphanedHeader(header: EncryBlockHeader): Future[Int] = runAsync(insertOrphanedHeaderQuery(header))

  private lazy val dataSource = new HikariDataSource
  if (settings.postgres.enabled) {
    dataSource.setJdbcUrl(settings.postgres.host)
    dataSource.setUsername(settings.postgres.user)
    dataSource.setPassword(settings.postgres.password)
    dataSource.setMaximumPoolSize(5)
  }

  private lazy val pgTransactor: HikariTransactor[IO] = HikariTransactor[IO](dataSource)

  private def runAsync(io: ConnectionIO[Int]): Future[Int] =
    if (settings.postgres.enabled) {
      (for {
        res <- io.transact(pgTransactor)
      } yield res)
        .unsafeToFuture()
        .recoverWith {
          case NonFatal(th) =>
            if (settings.logging.enableLogging)
              system.actorSelection("user/loggingActor") !
                LogMessage("Warn", s"Failed to perform db operation because of: $th", System.currentTimeMillis())
            Future.failed(th)
        }
    } else Future.successful(0)
}

object DBService {
  def apply(): DBService = new DBService
}
